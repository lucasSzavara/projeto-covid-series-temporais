source('./tendencia.R')
source('./sazonalidade.R')
source('./src/services/carregar_dados.R')
library(drc)
library('COVID19')
library(ggplot2)
library(zoo)
library(psych)
library(nloptr)
library(ggpubr)
library(stats)


#------------------------------------------------------------

# Salvar dados de cidades do Brasil em arquivos separados
x <- covid19(country=c('Brazil'), level=3, verbose=F, vintage = "2023-09-30")


erro_sazonalidade <- function(sigma, t, z) {
  eq <- 0
  d <- c()
  for (i in 1:7){
    zi <- z[c(rep(F, i-1), T, rep(F, 7-i))]
    eq <- eq + sum((zi/sigma[i] - 1) ^ 2)
    d <- c(d, 2/sigma[i] * sum(zi*(sigma[i] - zi)))
  }
  return( list("objective"=eq,
               "gradient"=d))
}


restricao <- function(sigma, t, z) {
  S <- c()
  jacs <- c()
  for (i in 1:7){
    S <- c(S, sigma[i]*sum(t[c(rep(F, i-1), T, rep(F, 7-i))]))
    jacs <- c(jacs, sum(t[c(rep(F, i-1), T, rep(F, 7-i))]))
  }
  return(list("constraints"=sum(S) - sum(t),
              "jacobian"=jacs))
}

sazonalidade_mg <- function(y, t) {
  saz <- c()
  termos <- c()
  for (i in 1:6){
    dias <- (y / t)[c(rep(F, i-1), T, rep(F, 7-i))]
    saz_i <- geometric.mean(c(dias[dias > 0], rep(1, sum(dias == 0))))
    saz <- c(saz, saz_i)
    termos <- c(termos, saz_i*sum(t[c(rep(F, i-1), T, rep(F, 7-i))]))
  }
  saz <- c(saz, (max(fit_tend) - sum(termos))/sum(t[c(rep(F, 6), T)]))
  saz
}


sazonalidade_mq <- function(y, t, s0=c()) {
  
  if(length(s0) != 7) {
    s0 <- c()
    for (i in 1:7){
      dias <- (y / t)[c(rep(F, i-1), T, rep(F, 7-i))]
      saz_i <- geometric.mean(c(dias[dias > 0], rep(1, sum(dias == 0))))
      s0 <- c(s0, saz_i)
    }
  }
  res0 <- nloptr( 
    x0=s0, 
    eval_f=erro_sazonalidade, 
    lb = rep(0, 7), 
    eval_g_eq = restricao,
    opts = list("algorithm" = "NLOPT_LD_AUGLAG",
                "xtol_rel"=1.0e-8,
                "print_level" = 0,
                "local_opts"=list("algorithm"="NLOPT_LD_SLSQP",
                                  "xtol_rel"=1.0e-5),
                "maxeval"=5000),
    t = t, 
    z = y / t
  )
  saz <- res0$solution
  saz
}


sazonalidade_mp <- function(y, t) {
  saz <- c()
  termos <- c()
  for (i in 1:7){
    ind <- c(rep(F, i-1), T, rep(F, 7-i))
    dias <- (y / t)[ind]
    saz_i <- weighted.mean(dias, t[ind])
    saz <- c(saz, saz_i)
    termos <- c(termos, saz_i*sum(t[ind]))
  }
  saz
}


locais <- read.csv('dados/estados_cidades.csv')
series <- c('deaths') # c("confirmed","deaths","vaccines")
estados <- sort(unique(locais$estados))
resultados <- tibble(
  estado="",
  cidade="",
  serie="",
  parametros=list(),
  tendencia=list(),
  saz=list(),
  eqm=Inf,
  method="",
  parametro=Inf,
  obj=list(),
  chute=list()
)
erros <- data.frame(estado=c(), cidade=c(), erro=c())
methods <- c('kriston', 'splines')
meta_params <- data.frame(method=c(rep('kriston', 4), rep('splines', 4)),
                          df=c(1:4 * 28, 2:5 * 10))
# resultados <- subset(resultados, cidade!=cid)
# estados <- 'São Paulo'
for (estado in estados) {
  cidades <- sort(unique(locais[locais$estados == estado,]$cidades))
  cat('Estado', estado, '\n Nº de cidades:', length(cidades))
  i <- 1
  for (serie in series) {
    for (cidade in cidades) {
      dados <- subset(x, administrative_area_level_2 == estado & administrative_area_level_3 == cidade)
      dados <- corrige(dados, serie)
      y <- dados[[serie]]
      n <- length(y)
      for (method in methods) {
        params <- meta_params[meta_params$method==method,]$df
        for (param in params) {
          tryCatch({
            if (i %% 50 == 0) {
              cat(i)
            }
            i <- i + 1
            # serie <- 'deaths'
            # estado <- 'São Paulo'
            # cidade <- 'Santo André'
            # dados <- subset(x, administrative_area_level_2 == estado & administrative_area_level_3 == cidade)
            # dados <- corrige(dados, serie)
            # y <- dados[[serie]]
            # n <- length(y)
            # source('./tendencia.R')
            print(param)
            print(method)
            resultado <- estima_tendencia(y,
                                          params=T, 
                                          param=param, 
                                          wcmethod=method, dates = dados$date[2:n])
            fit_tend <- resultado$tendencia
            params_tend <- list(resultado$params)
            chutes <- list(resultado$chutes)
            t <- diff(fit_tend)
            y_ <- diff(y)
            
            saz <- sazonalidade_mp(y_, t)
            eqm <-  sum((y_ - t*saz)^2) / n
            resultados <- resultados %>% 
              add_row(
                estado=estado,
                cidade=cidade,
                serie='deaths',
                parametros=params_tend,
                chute=chutes,
                eqm=eqm,
                saz=list(saz),
                tendencia=list(t),
                method=method,
                parametro=param,
                obj=list(resultado$obj),
              )
            }, error=function(err) {
              print(paste("Erro:  ",err))
              cat(estado, cidade, serie, '\n')
              erros[nrow(erros) + 1,] <- c(estado, cidade, err)
          })
        }
      }
    }
  }
}

tamanhos <- c(0, 1e4, 5e4, 1e5, 1e6, Inf)
nomes_tamanhos <- c(
  'muito_pequenas',
  'pequenas',
  'medias',
  'grandes',
  'muito_grandes'
)
cidades_faltantes <- tibble(
  estado="",
  cidade="",
)
for (uf in unique(resultados$estado)) {
  cidades <- sort(unique(resultados[resultados$estado == uf,]$cidade))
  cat('\nEstado', uf, '\n Nº de cidades:', length(cidades), '\n')
  pops <- subset(x, administrative_area_level_2 == uf) %>% group_by(administrative_area_level_2, administrative_area_level_3) %>%
    summarise(pop = mean(population))
  cidades_tamanho <- list(
    muito_pequenas=pops[pops$pop <= tamanhos[2],]$administrative_area_level_3,
    pequenas=pops[pops$pop <= tamanhos[3] & pops$pop > tamanhos[2],]$administrative_area_level_3,
    medias=pops[pops$pop <= tamanhos[4] & pops$pop > tamanhos[3],]$administrative_area_level_3,
    grandes=pops[pops$pop <= tamanhos[5] & pops$pop > tamanhos[4],]$administrative_area_level_3,
    muito_grandes=pops[pops$pop > tamanhos[5],]$administrative_area_level_3
  )
  i <- 1
  for(j in 1:length(nomes_tamanhos)) {
    dir.create(file.path('./graficos_metodos_ondas/', uf), showWarnings = FALSE)
    pdf(paste('./graficos_metodos_ondas/', uf, '/', nomes_tamanhos[j], '.pdf', sep=''),
        width=18, height=12)
    for(cid in cidades_tamanho[[j]]) {
      if (i %% 50 == 0) {
        cat(i, '\n')
      }
      if (dim(subset(resultados, estado == uf & cidade == cid))[1] == 0) {
        cidades_faltantes <- cidades_faltantes %>% add_row(
          estado=uf,
          cidade=cid,
        )
        next
      }
      i <- i + 1
      dados <- subset(x, administrative_area_level_2 == uf & administrative_area_level_3 == cid)
      # dados <- dados[dados[['deaths']] != 0,]
      dados <- corrige(dados, 'deaths')
      serie <- dados$deaths
      n <- length(serie)
      sub_resultados <- subset(resultados, estado==uf & cidade==cid)
      sub_res_kris <- subset(sub_resultados, method=='kriston')
      sub_res_spline <- subset(sub_resultados, method=='splines')
      graficos_kris <- list()
      graficos_spline <- list()
      if (nrow(sub_res_kris) != 0){
        for (m in 1:nrow(sub_res_kris)) {
          linha <- sub_res_kris[m,]
          tendencia <- linha$tendencia[[1]]
          if (length(dim(linha$chute[[1]])) == 0 ) {
            chute_inicial <- linha$chute[[1]]
          } else {
            chute_inicial <- linha$chute[[1]][1,]
          }
          ajuste_inicial <- calcula_tend(chute_inicial, 1:(length(serie)))$n
          saz <- linha$saz[[1]]
          eqm <- linha$eqm
          
          df <- as.data.frame(cbind(
            chute=diff(ajuste_inicial),
            saz=tendencia*saz,
            serie=diff(serie),
            t=2:n
          ))
          if (length(na.omit(linha$obj[[1]]$BF)) != 0) {
            wave_start <- which(linha$obj[[1]]$BF > 3)
            unique_waves <- c(wave_start[1])
            for (i in 2:length(wave_start)) {
              if(wave_start[i] - wave_start[i-1] > 1) {
                unique_waves <- c(unique_waves, wave_start[i])
              }
            }
            unified_waves <- c(unique_waves[1])
            for (i in 2:length(unique_waves)) {
              bfs <- linha$obj[[1]]$BF[unified_waves[length(unified_waves)]:unique_waves[i]]
              if (sum(is.na(bfs)) > 0 || sum(bfs < 1) > 0) {
                unified_waves <- c(unified_waves, unique_waves[i])
              }
            }
            
            unified_waves <- unified_waves - linha$parametro + 1
          } else {
            unified_waves <- c(1)
          }
          plot.cases <- ggplot(linha$obj[[1]], aes(date, cases)) +
            geom_line(aes(y=df$serie, color='Real'), size=0.2) +
            geom_line(aes(y=df$saz, color='Ajuste'), linewidth=0.25) +
            geom_line(aes(y=df$chute, color='Chute inicial'), linewidth=0.5) +
            annotate('text', label=paste('EQM: ', round(eqm, 5)), x=as.Date('2020-07-01'), y=max(diff(serie))*0.8) +
            scale_x_date(breaks='3 month') +
            scale_y_continuous(name="Novas mortes",
                               limits=c(0, NA)) +
            theme(
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "top") +
              scale_color_manual(values=c('#5555EE', '#FFA0A0', '#363636'))
          linha$obj[[1]]$date <- as.Date(substr(linha$obj[[1]]$date, 1, 10))
          plot_BF <- ggplot(linha$obj[[1]], aes(date, BF))+
            geom_hline(yintercept = c(1/3, 3), linetype=3, col="gray30") +
            geom_hline(yintercept = c(1/20, 20), linetype=2, col="gray30") +
            geom_hline(yintercept = c(1/150, 1, 150), linetype=1, col="gray30") +
            geom_vline(xintercept = linha$obj[[1]]$date[unified_waves], linetype=1, col='blue') +
            geom_line(aes(date, BF), col="black", size=1) +
            scale_x_date(name="Date (month/year)",
                         breaks='3 month',
                         date_labels = "%b/%y") +
            scale_y_continuous(name=paste(linha$parametro, 'Dias'),
                               trans="log10",
                               breaks=10^seq(-3, 3, 1),
                               lim=c(1/(10^3), 10^3),
                               labels=c(0.001, 0.01, 0.1, 1, 10, 100, 1000))
          g1 <- ggarrange(plot.cases, plot_BF, ncol=1)
          graficos_kris[[m]] <- g1
        }
      }
      if (nrow(sub_res_spline) != 0) {
        for (m in 1:nrow(sub_res_spline)) {
        linha <- sub_res_spline[m,]
        tendencia <- linha$tendencia[[1]]
        if (length(dim(linha$chute[[1]])) == 0 ) {
          chute_inicial <- linha$chute[[1]]
        } else {
          chute_inicial <- linha$chute[[1]][1,]
        }
        ajuste_inicial <- calcula_tend(chute_inicial, 1:(length(serie)))$n
        saz <- linha$saz[[1]]
        eqm <- linha$eqm
        
        df <- as.data.frame(cbind(
          chute=diff(ajuste_inicial),
          saz=tendencia*saz,
          serie=diff(serie),
          t=2:n
        ))
        
        plot.cases <- ggplot(df, aes(t, serie)) +
          geom_line(aes(color='Real'), size=0.2) +
          geom_line(aes(y=saz, color='Ajuste'), linewidth=0.25) +
          geom_line(aes(y=chute, color='Chute inicial'), linewidth=0.5) +
          annotate('text', label=paste('EQM: ', round(eqm, 5)), x=100, y=max(diff(serie))*0.8) +
          scale_y_continuous(name="Novas mortes",
                             limits=c(0, NA)) +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = 'none') +
          scale_color_manual(values=c('#5555EE', '#FFA0A0', '#363636'))
        
        spline_fit <- linha$obj[[1]]
        poly <- SmoothSplineAsPiecePoly(spline_fit)
        # Maximos e minimos das splines
        zeros  <- solve(poly, deriv=1)
        minimos_ <- zeros[predict(poly, zeros, deriv=2)>0]
        minimos <- c(1)
        if (length(minimos_) > 0) {
          for (i in 1:(length(minimos_) - 1)) {
            if(minimos_[i+1] - minimos_[i] > 100) {
              minimos <- c(minimos, minimos_[i])
            }
          }
        }
        
        plot_BF <- ggplot(data.frame(x=spline_fit$x, y=spline_fit$y), aes(x, y))+
          geom_vline(xintercept = minimos, linetype=1, col='blue') +
          geom_line(col="black", size=1) +
          scale_x_continuous(name=paste(linha$parametro, 'knots'))
        g1 <- ggarrange(plot.cases, plot_BF, ncol=1)
        graficos_spline[[m]] <- g1
        }
      }
      # g1 <- list()
      # for (k in 1:3) {
      #   g1[k] <- df %>%
      #     ggplot() +
      #     geom_line(aes(x=t, y=serie, color='Real'), linewidth=1) +
      #     geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5) +
      #     geom_line(aes(x=t, y=.data[[metodos_sazonalidade[k]]], color='Ajuste'), linewidth=0.5)
      #     # ggtitle(paste('Gráfico acumulado para a cidade:', cid)) +
      #     # ylab('Mortes por COVID-19')
      # }
      # g1 <- df %>%
      #       ggplot() +
      #       geom_line(aes(x=t, y=serie, color='Real'), linewidth=1) +
      #   geom_line(aes(x=t, y=.data[[metodos_sazonalidade[1]]], color='Ajuste'), linewidth=0.5) +
      #       geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5, linetype = "dashed")  +
      #   scale_color_manual(values=c('#5555EE', '#363636', '#FFA0A0')) 
      # g2 <- df %>%
      #   ggplot() +
      #   geom_line(aes(x=t, y=serie, color='Real'), linewidth=1) +
      #   geom_line(aes(x=t, y=.data[[metodos_sazonalidade[2]]], color='Ajuste'), linewidth=0.5) +
      #   geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5, linetype = "dashed")  +
      #   scale_color_manual(values=c('#5555EE', '#363636', '#FFA0A0'))
      # g3 <- df %>%
      #   ggplot() +
      #   geom_line(aes(x=t, y=serie, color='Real'), linewidth=1) +
      #   geom_line(aes(x=t, y=.data[[metodos_sazonalidade[3]]], color='Ajuste'), linewidth=0.5) +
      #   geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5, linetype = "dashed")  +
      #   scale_color_manual(values=c('#5555EE', '#363636', '#FFA0A0'))
      # 
      # g4 <- df_ %>%
      #   ggplot() +
      #   geom_line(aes(x=t, y=serie, color='Real'), size=0.2) +
      #   geom_line(aes(x=t, y=.data[[metodos_sazonalidade[1]]], color='Ajuste'), linewidth=0.25) +
      #   geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5, linetype = "dashed") +
      #   annotate('text', label=paste('EQM: ', round(eqm_mg, 5)), x=100, y=max(diff(serie))*0.8) +
      #   ylab('Mortes por COVID-19')  +
      #   scale_color_manual(values=c('#5555EE', '#363636', '#FFA0A0'))
      # 
      # g5 <- df_ %>%
      #   ggplot() +
      #   geom_line(aes(x=t, y=serie, color='Real'), size=0.2) +
      #   geom_line(aes(x=t, y=.data[[metodos_sazonalidade[2]]], color='Ajuste'), linewidth=0.25) +
      #   geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5, linetype = "dashed") +
      #   annotate('text', label=paste('EQM: ', round(eqm_mp, 5)), x=100, y=max(diff(serie))*0.8) +
      #   ylab('Mortes por COVID-19')  +
      #   scale_color_manual(values=c('#5555EE', '#363636', '#FFA0A0'))
      # 
      # g6 <- df_ %>%
      #   ggplot() +
      #   geom_line(aes(x=t, y=serie, color='Real'), size=0.2) +
      #   geom_line(aes(x=t, y=.data[[metodos_sazonalidade[3]]], color='Ajuste'), linewidth=0.25) +
      #   geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5, linetype = "dashed") +
      #   annotate('text', label=paste('EQM: ', round(eqm_mq, 5)), x=300, y=max(diff(serie))*0.8) +
      #   ylab('Mortes por COVID-19')  +
      #   scale_color_manual(values=c('#5555EE', '#363636', '#FFA0A0'))
      if (nrow(sub_res_kris) != 0 && nrow(sub_res_spline) != 0) {
      g_total <- ggarrange(ggarrange(plotlist = graficos_kris, 
                           # labels=nomes_metodos,
                           ncol=4,
                           nrow=1
                           ), ggarrange(plotlist = graficos_spline, 
                                        # labels=nomes_metodos,
                                        ncol=4,
                                        nrow=1
                           ), ncol=1, nrow=2)
      } else {
        g_total <- ggarrange(plotlist = c(graficos_spline, graficos_kris), 
                     # labels=nomes_metodos,
                     ncol=4,
                     nrow=1
        )
        }
      g_completo <- annotate_figure(g_total, 
                      top = text_grob(paste("Gráficos da cidade:", cid), face = "bold", size = 14))
      print(g_completo)
    }
    dev.off()
  }
}

resumo_metodos <- resultados %>% group_by(cidade, method) %>% summarise(N=n()/4, eqm_min=min(eqm), eqm_prop=min(eqm)/max(eqm), parametro=parametro[eqm==min(eqm, na.rm = T)])
