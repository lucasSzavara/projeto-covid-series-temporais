source('./tendencia.R')
source('./sazonalidade.R')
source('./metrics.R')
source('./src/services/carregar_dados.R')
library(drc)
library('COVID19')
library(ggplot2)
library(zoo)
library(psych)
library(nloptr)
library(ggpubr)
library(stats)
library(dplyr)
library(SimDesign)
library(purrr)


#------------------------------------------------------------

# Salvar dados de cidades do Brasil em arquivos separados
x <- covid19(country=c('Brazil'), level=3, verbose=F, vintage = "2023-09-30")

locais <- readRDS('./cidades_amostradas.rds')
locais <- locais %>% 
   mutate(dados=map2(administrative_area_level_2,
                     administrative_area_level_3,
                 function(e, c) {
                   dados <- x %>% filter(administrative_area_level_2 == e & 
                                 administrative_area_level_3 == c)
                  
                   corrige(dados, 'deaths')[c('deaths', 'population', 'date')]
                 }, .progress = list(
                   type = "iterator", 
                   clear = T,
                   show_after=0
                 )
                 )
          )
locais$y <- locais$dados %>% lapply(function(dados) dados$deaths)
locais$y_diff <- locais$y %>% lapply(function(y) diff(y))
locais$pop <- locais$dados %>% lapply(function(dados) dados$population[1])
locais$date <- locais$dados %>% lapply(function(dados) dados$date)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


hiperparametros <- createDesign(method=c('splines_method'),
                         param=c(5, 10, 20, 30),
                         weekly=c(T, F),
                         opt=c('nloptr', 'nls'))
cenarios <- cross_join(locais, hiperparametros)
cenarios <- cenarios %>% 
  mutate(fit_tendencia=pmap(
    list(y, pop, param, method, date, weekly, opt),
    function(y, pop, param, method, date, weekly, opt) {
      tryCatch(quietly(estima_tendencia)(y,
                                pop,
                                params=T, 
                                param=param, 
                                wcmethod=method,
                                dates = date,
                                weekly=weekly, 
                                optimization_method=opt
                                )$result, error=function(err) as.character(err)
      )
    }, .progress = list(
      type = "iterator", 
      clear = T,
      show_after=0
    )
  ))

cenarios$t_diff <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(diff(t$tendencia),
                              error=function(err) as.character(err)))

cenarios <- cenarios %>% 
  mutate(saz=map2(y_diff, t_diff, 
                  function(y, t) {
                    tryCatch(
                      sazonalidade_mp(y, t),
                      error=function(err) as.character(err)
                    )
                  }, .progress = list(
                    type = "iterator", 
                    clear = T,
                    show_after=0
                  )
                )
  )

cenarios <- cenarios %>% 
  mutate(y_hat=map2(t_diff, saz,
                function(t, saz) {
                  tryCatch(
                    t * saz,
                    error=function(err) as.character(err)
                  )
                }, .progress = list(
                  type = "iterator", 
                  clear = T,
                  show_after=0
                )
  ))

cenarios <- cenarios %>% 
  mutate(mad.mean.daily=map2(y_hat, y_diff,
                       function(y_hat, y_diff) {
                         tryCatch(
                           mad.mean(y_diff, y_hat),
                           error=function(err) as.character(err)
                         )
                       }, .progress = list(
                         type = "iterator", 
                         clear = T,
                         show_after=0
                       )
          ),
        mad.mean.cummulative=map2(y_hat, y,
                                  function(y_hat, y) {
                                    tryCatch(
                                      mad.mean(y, diffinv(y_hat, xi=0)),
                                      error=function(err) as.character(err)
                                    )
                                  }, .progress = list(
                                    type = "iterator", 
                                    clear = T,
                                    show_after=0
                                  )
        )
  )


cenarios$chute_inicial <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(calcula_tend(t$chutes[1,], 1:length(t$tendencia))$n,
                              error=function(err) as.character(err)))

cenarios$erro <- is.na(cenarios$mad.mean.cummulative)
saveRDS(cenarios, './cenarios_amostrados_splines.rds')


hiperparametros <- createDesign(method=c('kriston_method'),
                                param=c(1:4 * 28),
                                weekly=c(T, F),
                                opt=c('nloptr', 'nls'))
hiperparametros[hiperparametros$weekly, 'param'] = hiperparametros[hiperparametros$weekly, 'param'] / 7
cenarios <- cross_join(locais, hiperparametros)
cenarios <- cenarios %>% 
  mutate(fit_tendencia=pmap(
    list(y, pop, param, method, date, weekly, opt),
    function(y, pop, param, method, date, weekly, opt) {
      tryCatch(quietly(estima_tendencia)(y,
                                         pop,
                                         params=T, 
                                         param=param, 
                                         wcmethod=method,
                                         dates = date,
                                         weekly=weekly, 
                                         optimization_method=opt
      )$result, error=function(err) as.character(err)
      )
    }, .progress = list(
      type = "iterator", 
      clear = T,
      show_after=0
    )
  ))


# cenarios$tendencia <- cenarios$fit_tendencia %>% 
#   lapply(function(t) tryCatch(t$tendencia,
#                               error=function(err) as.character(err)))
cenarios$t_diff <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(diff(t$tendencia),
                              error=function(err) as.character(err)))

cenarios <- cenarios %>% 
  mutate(saz=map2(y_diff, t_diff, 
                  function(y, t) {
                    tryCatch(
                      sazonalidade_mp(y, t),
                      error=function(err) as.character(err)
                    )
                  }, .progress = list(
                    type = "iterator", 
                    clear = T,
                    show_after=0
                  )
  )
  )

cenarios <- cenarios %>% 
  mutate(y_hat=map2(t_diff, saz,
                    function(t, saz) {
                      tryCatch(
                        t * saz,
                        error=function(err) as.character(err)
                      )
                    }, .progress = list(
                      type = "iterator", 
                      clear = T,
                      show_after=0
                    )
  ))

cenarios <- cenarios %>% 
  mutate(mad.mean.daily=map2(y_hat, y_diff,
                             function(y_hat, y_diff) {
                               tryCatch(
                                 mad.mean(y_diff, y_hat),
                                 error=function(err) as.character(err)
                               )
                             }, .progress = list(
                               type = "iterator", 
                               clear = T,
                               show_after=0
                             )
  ),
  mad.mean.cummulative=map2(y_hat, y,
                            function(y_hat, y) {
                              tryCatch(
                                mad.mean(y, diffinv(y_hat, xi=0)),
                                error=function(err) as.character(err)
                              )
                            }, .progress = list(
                              type = "iterator", 
                              clear = T,
                              show_after=0
                            )
  )
  )


cenarios$chute_inicial <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(calcula_tend(t$chutes[1,], 1:length(t$tendencia))$n,
                              error=function(err) as.character(err)))

cenarios$erro <- is.na(cenarios$mad.mean.cummulative)
saveRDS(cenarios, './cenarios_amostrados_kriston.rds')

cenarios <- readRDS("./cenarios_21_06.rds")
cenarios_ <- readRDS("./cenarios_26_06.rds")
cenarios <- rbind(cenarios, cenarios_)
rm(cenarios_)
cenarios <- cenarios[,c('y', 'y_diff', 'method', 'param', 'weekly', 'opt',
                        'tendencia', 't_diff', 'y_hat', 'mad.mean.daily',
                        'mad.mean.cummulative', 'chute_inicial')]
saveRDS(cenarios, './cenarios.rds')
cenarios <- readRDS('./cenarios.rds')

cenarios <- cenarios[,c('method', 'param', 'weekly', 'opt',
                        'mad.mean.daily',
                        'mad.mean.cummulative', 'y_hat', 'y_diff')]
saveRDS(cenarios, './cenarios_medium.rds')

cenarios <- readRDS('./cenarios_light.rds')

cenarios <- cenarios %>% 
  mutate(umbrae.total=map2(y_hat, y_diff,
                             function(y_hat, y_diff) {
                               tryCatch(
                                 umbrae(y_diff, y_hat),
                                 error=function(err) as.character(err)
                               )
                             }, .progress = list(
                               type = "iterator", 
                               clear = T,
                               show_after=0
                             )
  )
  )
cenarios$erro <- is.na(cenarios$mad.mean.cummulative)
cenarios %>% group_by(method) %>% summarise(tx_erro=mean(erro))
cenarios %>% filter(method=='splines_method') %>% nrow()
cenarios$param_ <- paste(cenarios$method, cenarios$param)
logit <- glm(erro ~ (method  + opt)^2,
             data=cenarios,
             # subset = cenarios$method=='splines_method',
             family='binomial')

logit.sumario <- summary(logit)
saveRDS(logit, 'logit_erros_spline.rds')
anova_erros <- anova(logit)
anova_erros
cenarios <- cenarios %>% 
  filter(!erro)

cenarios$mad.mean.daily <- unlist(cenarios$mad.mean.daily)
modelo.mad.daily <- lm(mad.mean.daily ~ (method + opt)^2,
                       # subset = cenarios$method=='splines_method',
             data=cenarios)
modelo.mad.sumario <- summary(modelo.mad.daily)

coefs <- data.frame(logit=logit.sumario$coefficients, mad=modelo.mad.sumario$coefficients)
coefs[, c('logit.Estimate', 'mad.Estimate')] %>% View()
cor(coefs$logit.Estimate, coefs$mad.Estimate)

coefs %>% ggplot(aes(x=logit.Estimate, y=mad.Estimate, label=rownames(coefs))) +
  geom_point() +
  scale_y_continuous(limits=c(-0.15, 0.25))+
  scale_x_continuous(limits=c(-2.5, 5)) +
  geom_text(aes(size = sqrt(logit.Estimate^2 + mad.Estimate^2))) +
  guides(size="none")



saveRDS(modelo.mad.daily, 'modelo.mad.daily.rds')
anova_mad <- anova(modelo.mad.daily)
anova_mad
modelo.mad.daily.sem.inter <- lm(mad.mean.daily ~ param_ + weekly + opt,
                       data=cenarios)
summary(modelo.mad.daily.sem.inter)
saveRDS(modelo.mad.daily.sem.inter, 'modelo.mad.daily.sem.inter.rds')
anova_mad_sem_inter <- anova(modelo.mad.daily.sem.inter)
anova_mad_sem_inter


cenarios <- cenarios[sample(1:nrow(cenarios), size = round(nrow(cenarios)/100)),]

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
          umbrae_s <- linha$umbrae_s
          
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
            if (length(unique_waves) > 1) {
              for (i in 2:length(unique_waves)) {
                bfs <- linha$obj[[1]]$BF[unified_waves[length(unified_waves)]:unique_waves[i]]
                if (sum(is.na(bfs)) > 0 || sum(bfs < 1) > 0) {
                  unified_waves <- c(unified_waves, unique_waves[i])
                }
              }
            }
            
            unified_waves <- unified_waves - linha$parametro + 1
          } else {
            unified_waves <- c(1)
          }
          plot.cases <- ggplot(linha$obj[[1]], aes(1:(length(serie)), cases)) +
            geom_line(aes(y=df$serie, color='Real'), size=0.2) +
            geom_line(aes(y=df$saz, color='Ajuste'), linewidth=0.25) +
            geom_line(aes(y=df$chute, color='Chute inicial'), linewidth=0.5) +
            annotate('text', label=paste('umbrae_s: ', round(umbrae_s, 5)), x=as.Date('2020-07-01'), y=max(diff(serie))*0.8) +
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
          plot_BF <- ggplot(linha$obj[[1]], aes(1:(length(serie)), BF))+
            geom_hline(yintercept = c(1/3, 3), linetype=3, col="gray30") +
            geom_hline(yintercept = c(1/20, 20), linetype=2, col="gray30") +
            geom_hline(yintercept = c(1/150, 1, 150), linetype=1, col="gray30") +
            geom_vline(xintercept = (1:(length(serie)))[unified_waves], linetype=1, col='blue') +
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
        umbrae_s <- linha$umbrae_s
        
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
          annotate('text', label=paste('umbrae_s: ', round(umbrae_s, 5)), x=100, y=max(diff(serie))*0.8) +
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
      #   annotate('text', label=paste('umbrae_s: ', round(umbrae_s_mg, 5)), x=100, y=max(diff(serie))*0.8) +
      #   ylab('Mortes por COVID-19')  +
      #   scale_color_manual(values=c('#5555EE', '#363636', '#FFA0A0'))
      # 
      # g5 <- df_ %>%
      #   ggplot() +
      #   geom_line(aes(x=t, y=serie, color='Real'), size=0.2) +
      #   geom_line(aes(x=t, y=.data[[metodos_sazonalidade[2]]], color='Ajuste'), linewidth=0.25) +
      #   geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5, linetype = "dashed") +
      #   annotate('text', label=paste('umbrae_s: ', round(umbrae_s_mp, 5)), x=100, y=max(diff(serie))*0.8) +
      #   ylab('Mortes por COVID-19')  +
      #   scale_color_manual(values=c('#5555EE', '#363636', '#FFA0A0'))
      # 
      # g6 <- df_ %>%
      #   ggplot() +
      #   geom_line(aes(x=t, y=serie, color='Real'), size=0.2) +
      #   geom_line(aes(x=t, y=.data[[metodos_sazonalidade[3]]], color='Ajuste'), linewidth=0.25) +
      #   geom_line(aes(x=t, y=tendencia, color='Tendencia'), linewidth=0.5, linetype = "dashed") +
      #   annotate('text', label=paste('umbrae_s: ', round(umbrae_s_mq, 5)), x=300, y=max(diff(serie))*0.8) +
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
resumo_metodos <- resultados %>% 
  filter(umbrae_s != -1) %>% 
  group_by(cidade, method, pop) %>% 
  summarise(N=n()/4,
            umbrae_s_min=min(umbrae_s),
            umbrae_s_prop=min(umbrae_s)/max(umbrae_s),
            melhoria_semanal=mean(umbrae_comparison),
            parametro=parametro[umbrae_s==min(umbrae_s, na.rm = T)]
            )
# discretize
resumo_metodos <- resumo_metodos %>% 
  mutate(categoria=ifelse(pop < 1e4, 'muito pequena', 
                          ifelse(pop < 5e4, 'pequena',
                                 ifelse(pop < 1e5, 'media',
                                        ifelse(pop < 1e6, 'grande', 'muito grande'))))) %>% 
  mutate(categoria=factor(categoria, levels=c(
    'muito pequena',
    'pequena',
    'media',
    'grande',
    'muito grande'
  )))

resumo_metodos %>%
# filter(categoria %in% c('muito pequena', 'pequena')) %>% 
  ggplot() +
  geom_boxplot(aes(x=categoria, y=melhoria_semanal, fill=method), notch = T) +
  coord_cartesian(ylim=c(0, 5))

resumo <- resumo_metodos %>% group_by(cidade, method) %>% 
  filter(!is.na(umbrae_s_min)) %>% 
  # filter(smape==min(smape)) %>% 
  filter(parametro == min(parametro)) %>% 
  group_by(categoria, method) %>% 
  summarize(umbrae_s.li=quantile(umbrae_s_min, .25),
            umbrae_s.ls=quantile(umbrae_s_min, .75),
            umbrae_s.md=median(umbrae_s_min)
            )

resumo %>% ggplot() +
  geom_segment(aes(x=umbrae_s.li, xend=umbrae_s.ls, colour=method, y=method, yend=method)) +
  geom_point(aes(x=umbrae_s.md, y=method, colour=method)) +
  facet_wrap(~categoria, ncol=5, scales='free_x')
write.csv(resumo_metodos, './resumo_14_06.csv')
load('./cenarios_21_06.rds')
