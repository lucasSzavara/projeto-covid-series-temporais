source('./tendencia.R')
source('./sazonalidade.R')
source('./src/services/carregar_dados.R')
library(drc)
library('COVID19')
library(ggplot2)
library(zoo)

#------------------------------------------------------------

# Salvar dados de cidades do Brasil em arquivos separados
x <- covid19(country=c('Brazil'), level=3, verbose=F, vintage = "2023-09-30")


w <- 90
estimacao <- function(y, datas, width=w, tol=1e-8, max_iter=500) {
  serie <- 'deaths'
  estado <- 'São Paulo'
  cidade <- 'São Paulo'
  dados <- subset(x, administrative_area_level_2 == estado & administrative_area_level_3 == cidade)
  dados <- corrige(dados, serie)
  y <- dados[[serie]]
  width <- 90
  datas <- dados$date[(length(dados[[serie]])-length(y) + 2):length(dados$date)]
  tol <- 1e-7
  max_iter <- 500
  resultado <- estima_tendencia(y,
                                params=T)
  fit_tend <- resultado$tendencia
  params_tend <- resultado$params
  N <- length(params_tend) / 3
  # var_estabilizada <- padroniza_variancia(y-fit_tend, width=width, invertible=T)
  # y_ <- var_estabilizada$serie
  # sd <- var_estabilizada$sd
  # saz <- estima_sazonalidade(ifelse(is.infinite(diff(y) / diff(fit_tend)), 1, diff(y) / diff(fit_tend)), datas)
  # saz <- (saz %>%
  #           fitted())$.fitted
  saz <- c()
  for (i in 1:7){
    saz <- c(saz,mean((diff(y) / diff(fit_tend))[c(rep(F, i-1), T, rep(F, 7-i))]))
  }
  saz <- saz + (7-sum(saz))/7
  saz <- (rep_len(saz, length(y) - 1))
  if (sum(saz < 0) > 0) {
    saz <- saz + abs(min(saz))
  }
  p <- c(params_tend[[1]], sd, saz[1:7])
  for (i in 1:max_iter) {
    new_result = ajusta_modelo(cumsum(c(1, diff(y) / saz)), N, params_tend) # estima_tendencia(cumsum(c(1, diff(y) / saz)), params=T)
    fit_tend_new <- predict(new_result) #$tendencia
    # params_new <- list(new_result$params)
    params_new = summary(new_result)$parameters[,'Estimate']
    # var_ <- padroniza_variancia(y-fit_tend_new, width=width, invertible=T)
    # y_ <- var_$serie
    # sd_new <- var_$sd
    
    saz_new <- c()
    for (i in 1:7){
      saz_new <- c(saz,mean((diff(y) / diff(fit_tend_new))[c(rep(F, i-1), T, rep(F, 7-i))]))
    }
    saz_new <- saz_new + (7-sum(saz_new))/7
    saz_new <- (rep_len(saz_new, length(y) - 1))
    # saz_new <- (saz_new %>%
    #           fitted())$.fitted
    # if (sum(saz_new < 0) > 0) {
    #   saz_new <- saz_new + abs(min(saz_new))
    # }
    p_new <- c(params_new[[1]], sd_new, saz_new[1:7])
    if (length(p) == length(p_new) && sqrt(sum((p - p_new) ^ 2)) <= tol) {
      params_tend <- params_new
      saz <- saz_new
      # sd <- sd_new
      fit_tend <- fit_tend_new
      p <- p_new
      break
      return (list(
        param_tend=params_new,
        # sd=sd_new,
        saz=saz_new
      ))
    }
    params_tend <- params_new
    saz <- saz_new
    # sd <- sd_new
    fit_tend <- fit_tend_new
    p <- p_new
  }
  g1 <- as.data.frame(cbind(ajuste=c(1, cumsum(diff(fit_tend)*saz)), serie=y, t=1:length(y))) %>%
    ggplot() +
    geom_line(aes(x=t, y=serie, color='Real'), linewidth=1) +
    geom_line(aes(x=t, y=ajuste, color='Ajuste'), linewidth=0.5) +
    ggtitle(paste('Gráfico acumulado para a cidade:', cidade)) +
    ylab('Mortes por COVID-19')
  g2 <- as.data.frame(cbind(ajuste=diff(fit_tend)*saz, serie=diff(y), t=(2):length(y))) %>%
    ggplot() +
    geom_line(aes(x=t, y=serie, color='Real')) +
    geom_line(aes(x=t, y=ajuste, color='Ajuste'), linewidth=0.5) +
    ggtitle(paste('Gráfico diário para a cidade:', cidade)) +
    ylab('Mortes por COVID-19')
  print(g1)
  print(g2)
}

locais <- read.csv('dados/estados_cidades.csv')
series <- c('deaths') # c("confirmed","deaths","vaccines")
estados <- sort(unique(locais$estados))
resultados <- tibble(
  estado="",
  cidade="",
  serie="",
  parametros=list(),
  ajuste=list(),
  eqm=Inf,
  epam=Inf,
  elqm=Inf,
  eqm_tendencia=Inf,
  epam_tendencia=Inf,
  elqm_tendencia=Inf,
  chutes=list()
)
erros <- data.frame(estado=c(), cidade=c(), erro=c())
for (estado in estados) {
  cidades <- sort(unique(locais[locais$estados == estado,]$cidades))
  cat('Estado', estado, '\n Nº de cidades:', length(cidades))
  i <- 1
  for (serie in series) {
    for (cidade in cidades) {
      dados <- subset(x, administrative_area_level_2 == estado & administrative_area_level_3 == cidade)
      tryCatch({
          if (i %% 50 == 0) {
              cat(i)
          }
          i <- i + 1
          # serie <- 'deaths'
          # estado <- 'Paraná'
          # cidade <- 'Campo Mourão'
          # dados <- subset(x, administrative_area_level_2 == estado & administrative_area_level_3 == cidade)
          dados <- corrige(dados, serie)
          y <- dados[[serie]]
          n <- length(y)
          # source('./tendencia.R')
          resultado <- estima_tendencia(y,
                                        params=T, 
                                        df=ifelse(sum(
                                          dados$population > 500000
                                          ) > 0, 
                                          yes = 50, 
                                          no=40))
          fit_tend <- resultado$tendencia
          params_tend <- list(resultado$params)
          chutes <- list(resultado$chutes)
          var_estabilizada <- padroniza_variancia(y-fit_tend, width=w, invertible=T)
          y_ <- var_estabilizada$serie
          sd <- var_estabilizada$sd
          saz <- estima_sazonalidade(y_, dados$date[w:length(dados$date)])
          saz <- (saz %>%
                 fitted())['.fitted']
          fit <- fit_tend[w:length(dados$date)] + saz * sd # * p
          eqm_t <-  sum((y - fit_tend)^2) / n
          epam_t <- sum(abs((y-fit_tend)/y)) / n
          elqm_t <- sum((log(y) - log(fit_tend))^2) / n
          eqm <-  sum((y[w:length(dados$date)] - fit)^2) / n
          epam <- sum(abs((y[w:length(dados$date)]-fit)/y)) / n
          elqm <- sum((log(y[w:length(dados$date)]) - log(fit))^2) / n
          resultados <- resultados %>% 
            add_row(
              estado=estado,
              cidade=cidade,
              serie='deaths',
              parametros=params_tend,
              eqm_tendencia=eqm_t,
              epam_tendencia=epam_t,
              elqm_tendencia=elqm_t,
              eqm=eqm,
              epam=epam,
              elqm=elqm,
              ajuste=as.list(fit),
              chutes=chutes
              )
        }, error=function(err) {
          print(paste("MY_ERROR:  ",err))
          cat(estado, cidade, serie, '\n')
          erros[nrow(erros) + 1,] <- c(estado, cidade, err)
        }
      )
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
  cidades_tamanho <- list(
    muito_pequenas=c(),
    pequenas=c(),
    medias=c(),
    grandes=c(),
    muito_grandes=c()
  )
  for (cidade in cidades) {
    pop <- mean(subset(x, 
                       administrative_area_level_2 == uf & administrative_area_level_3 == cidade)['population'][[1]])
    for(j in 2:(length(tamanhos))) {
      if(tamanhos[j - 1] <= pop && pop < tamanhos[j]){
        cidades_tamanho[[j - 1]] <- append(cidades_tamanho[[j - 1]], cidade)
      }
    }
  }
  i <- 1
  for(j in 1:length(nomes_tamanhos)) {
    pdf(paste('./graficos/', uf, '/', nomes_tamanhos[j], '.pdf', sep=''))
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
      serie <- dados$deaths[w:length(dados$deaths)]
      sub_resultados <- subset(resultados, estado==uf & cidade==cid)
      params <- sub_resultados$parametros[[1]]
      if (length(dim(sub_resultados$chutes[[1]])) == 0 ) {
        chute_inicial <- sub_resultados$chutes[[1]]
      } else {
        chute_inicial <- sub_resultados$chutes[[1]][1,]
      }
      ajuste_inicial <- calcula_tend(chute_inicial, w:(length(serie)+w-1))$n
      ajuste <- sub_resultados$ajuste$.fitted
      g1 <- as.data.frame(cbind(ajuste=ajuste, ajuste_inicial=ajuste_inicial, serie=serie, t=w:(length(serie)+w-1))) %>%
        ggplot() +
        geom_line(aes(x=t, y=serie, color='Real'), linewidth=1) +
        geom_line(aes(x=t, y=ajuste, color='Ajuste'), linewidth=0.5) +
        geom_line(aes(x=t, y=ajuste_inicial, color='Ajuste Inicial'), linewidth=0.5) +
        ggtitle(paste('Gráfico acumulado para a cidade:', cid)) +
        ylab('Mortes por COVID-19') +
        annotate('text', label=paste('Valores ajustados: \n', paste(names(params), round(params, 2), sep = ":", collapse = "\n")), x=length(serie) * 0.8, y=max(serie) * 0.5) +
        annotate('text', label=paste('Chutes Iniciais: \n', paste(names(chute_inicial), round(chute_inicial, 2), sep = ":", collapse = "\n")), x=length(serie) * 0.15, y=max(serie) * 0.7)
      g2 <- as.data.frame(cbind(ajuste=diff(ajuste), ajuste_inicial=diff(ajuste_inicial), serie=diff(serie), t=(w+1):(length(serie)+w-1))) %>%
        ggplot() +
        geom_line(aes(x=t, y=serie, color='Real')) +
        geom_line(aes(x=t, y=ajuste, color='Ajuste'), linewidth=0.5) +
        geom_line(aes(x=t, y=ajuste_inicial, color='Ajuste Inicial'), linewidth=0.5) +
        ggtitle(paste('Gráfico diário para a cidade:', cid)) +
        ylab('Mortes por COVID-19')
      print(g1)
      print(g2)
    }
    dev.off()
  }
}

