library(dplyr)
library(ggplot2)
library('COVID19')
library(splitstackshape)
source('./src/services/carregar_dados.R')
library(ggpubr)
source('./metrics.R')
library(SimDesign)
library(purrr)
library(stats)
source('./sazonalidade.R')
source('./tendencia.R')

set.seed(42)
# Amostra cidades para analisar:
x <- covid19(country=c('Brazil'), level=3, verbose=F, vintage = "2023-09-30")
x <- x %>% 
  mutate(categoria=ifelse(population < 1e4, 'muito pequena', 
                          ifelse(population < 5e4, 'pequena',
                                 ifelse(population < 1e5, 'media',
                                        ifelse(population < 1e6, 'grande', 'muito grande'))))) %>% 
  mutate(categoria=factor(categoria, levels=c(
    'muito pequena',
    'pequena',
    'media',
    'grande',
    'muito grande'
  )))
cidades <- x[, c('administrative_area_level_2',
                 'administrative_area_level_3',
                 'categoria', 'population')] %>% 
  distinct()
saveRDS(cidades, './lista_cidades.rds')
cidades <- readRDS('./lista_cidades.rds')
n <- 500
Nh <- (cidades %>% group_by(categoria) %>% summarise(Nh=n()))
N <- length(cidades$administrative_area_level_3)
Nh <- Nh %>% mutate(Wh=Nh/N)
Wh <- Nh$Wh
n_h <- Wh * n
names(n_h) <- c(
  'muito pequena',
  'pequena',
  'media',
  'grande',
  'muito grande'
)
amostra_piloto <- cidades %>% stratified('categoria', pmax(round(n_h), 1), replace=F)
saveRDS(amostra_piloto, './locais.rds')
rm(cidades, amostra_piloto, n_h, Nh, Wh, n, N)


# Ajusta modelo nas cidades amostradas

locais <- readRDS('./locais.rds')
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
saveRDS(locais, './locais.rds')



hiperparametros <- createDesign(method=c('splines_method'),
                                param=c(5, 10, 20, 30),
                                weekly=c(T, F),)
cenarios <- cross_join(locais, hiperparametros)
cenarios <- cenarios %>% 
  mutate(fit_tendencia=pmap(
    list(y, pop, param, method, date, weekly),
    function(y, pop, param, method, date, weekly) {
      tryCatch(quietly(estima_tendencia)(y,
                                         pop,
                                         params=T, 
                                         param=param, 
                                         wcmethod=method,
                                         dates = date,
                                         weekly=weekly, 
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
cenarios$loglik <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$loglik,
                              error=function(err) as.character(err)))
cenarios$aic <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$aic,
                              error=function(err) as.character(err)))
cenarios$bic <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$bic,
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
                                weekly=c(T, F))
hiperparametros[hiperparametros$weekly, 'param'] = hiperparametros[hiperparametros$weekly, 'param'] / 7
cenarios <- cross_join(locais, hiperparametros)
cenarios <- cenarios %>% 
  mutate(fit_tendencia=pmap(
    list(y, pop, param, method, date, weekly),
    function(y, pop, param, method, date, weekly) {
      tryCatch(quietly(estima_tendencia)(y,
                                         pop,
                                         params=T, 
                                         param=param, 
                                         wcmethod=method,
                                         dates = date,
                                         weekly=weekly, 
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
cenarios$loglik <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$loglik,
                              error=function(err) as.character(err)))
cenarios$aic <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$aic,
                              error=function(err) as.character(err)))
cenarios$bic <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$bic,
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
rm(cenarios)
rm(x)



# Compara ajustes
get_box_stats <- function(y) {
  return(data.frame(
    y = 2.5,
    label = paste(
      "Count =", length(y), "\n",
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n"
    )
  ))
}


get_count_stats <- function(y) {
  return(data.frame(
    y = 2.5,
    label = length(y)
    )
  )
}

# cenarios <- readRDS('./cenarios_light.rds')
# cenarios$erro <- is.na(cenarios$mad.mean.cummulative)
# cenarios_splines <- cenarios %>% filter(method == 'splines_method')
cenarios_splines <- readRDS('./cenarios_amostrados_splines.rds')
cenarios_splines$param <- unlist(as.factor(cenarios_splines$param))
cenarios_splines$mad.mean.daily <- as.numeric(cenarios_splines$mad.mean.daily)

cenarios_splines %>% filter(!erro) %>%
  ggplot(aes(x=param, y=mad.mean.daily, fill=weekly)) +
  geom_boxplot() +
  stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9)

cenarios_splines <- cenarios_splines %>% 
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

cenarios_splines %>% filter(!erro) %>%
  ggplot(aes(x=param, y=mad.mean.daily, fill=weekly)) +
  geom_boxplot() +
  stat_summary(fun.data = get_count_stats,
               geom = "text",
               hjust = 0.5,
               vjust = 0.9,
               position = position_dodge(width = 0.9))

# Para o spline, parece que quanto mais converge, piores as métricas de erro.
# É difícil escolher uma boa configuração



# cenarios_kriston <- cenarios %>% filter(method == 'kriston_method')
cenarios_kriston <- readRDS('./cenarios_amostrados_kriston.rds')
cenarios_kriston$param <- unlist(as.factor(cenarios_kriston$param))
cenarios_kriston$mad.mean.daily <- as.numeric(cenarios_kriston$mad.mean.daily)

cenarios_kriston %>% filter(!erro) %>%
  ggplot(aes(x=param, y=mad.mean.daily, fill=weekly)) +
  geom_boxplot() +
  stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9)

cenarios_kriston <- cenarios_kriston %>% 
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

cenarios_kriston %>% filter(!erro) %>%
  ggplot(aes(x=param, y=mad.mean.daily, fill=weekly)) +
  geom_boxplot() +
  stat_summary(fun.data = get_count_stats,
               geom = "text",
               hjust = 0.5,
               vjust = 0.9,
               position = position_dodge(width = 0.75))

# Para o kriston, o melhor parece ser semanal, usando 4 semanas. 
# O NLOPTR parece convergir com mais frequencia, mas para essa configuração tem 
# as mesmas métricas de erro


locais <- readRDS('./locais.rds')
locais$pop <- unlist(locais$population)
locais <- locais %>% arrange(desc(pop))
cenarios_splines <- readRDS('./cenarios_amostrados_splines.rds')
cenarios_splines$erro <- is.na(cenarios_splines$mad.mean.cummulative)

# gera_grafico.fn <- function(cen, date_, y, method) {
#   fn <- function(p) {
#     # p <- 30
#     # cenario_total <- cenarios_splines[cenarios_splines$administrative_area_level_2 == 'Alagoas' &
#     #                                     cenarios_splines$administrative_area_level_3 == 'Maceió',]
#     # cen <- cenario_total[!cenario_total$weekly, ]
#     # y <- cen[1, 'y_diff'][[1]][[1]]
#     # date_ <- cen[1, 'date'][[1]][[1]]
#     # cen <- cen %>% mutate(
#     #   y_hat=ifelse(erro, 0, y_hat),
#     #   chute_inicial=ifelse(erro, 0, chute_inicial)
#     # )
#     cen.param <- cen[cen$param == p,]
#     cen.param <- cen.param %>% mutate(
#       mad.mean.daily=ifelse(erro, Inf, mad.mean.daily),
#     )
#     if (nrow(cen.param[!cen.param$erro]) == 0) {
#       df <- data.frame('Real'=cumsum(y),
#                        'Dia'=date_[2:length(date_)])
#       plot <- ggplot(df) +
#         geom_point(aes(x=Dia, y=Real, color='Real'), alpha=0.3) +
#         theme(
#           axis.title.x = element_blank(),
#           axis.text.x = element_blank(),
#           axis.ticks.x=element_blank(),
#           legend.position = 'none') +
#         scale_color_manual(values=c('#363636')) +
#         ggtitle(paste('Spline com parametro', p))
#       return(plot)
#     }
#     y_hat.nls <- cen.param[cen.param$opt == 'nls', 'y_hat'][[1]][[1]]
#     y_hat.nloptr <- cen.param[cen.param$opt == 'nloptr', 'y_hat'][[1]][[1]]
#     
#     mad.mean.daily.nls <- cen.param[cen.param$opt == 'nls', 'mad.mean.daily'][[1]][[1]]
#     mad.mean.daily.nloptr <- cen.param[cen.param$opt == 'nloptr', 'mad.mean.daily'][[1]][[1]]
#     
#     aic.nls <- cen.param[cen.param$opt == 'nls', 'aic'][[1]][[1]]
#     aic.nloptr <- cen.param[cen.param$opt == 'nloptr', 'aic'][[1]][[1]]
#     
#     bic.nls <- cen.param[cen.param$opt == 'nls', 'bic'][[1]][[1]]
#     bic.nloptr <- cen.param[cen.param$opt == 'nloptr', 'bic'][[1]][[1]]
#     
#     umbrae_res <- umbrae(y, y_hat.nloptr, y_hat.nls)
#     y_hat.0 <- diff(cen.param[!cen.param$erro][1, 'chute_inicial'][[1]][[1]])
#     df <- data.frame('Real'=cumsum(y),
#                      'Chute Inicial'=cumsum(y_hat.0),
#                      'NLS'=cumsum(y_hat.nls),
#                      'Verossimilhança'=cumsum(y_hat.nloptr),
#                      'Dia'=date_[2:length(date_)])
#     plot <- ggplot(df) +
#       geom_point(aes(x=Dia, y=Real, color='Real'), alpha=0.3) +
#       geom_line(aes(x=Dia, y=NLS, color='Normal')) +
#       geom_line(aes(x=Dia, y=Verossimilhança, color='Binomial')) +
#       geom_line(aes(x=Dia, y=Chute.Inicial, color='Chute Inicial')) +
#       theme(
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x=element_blank(),
#         legend.position = 'none') +
#       scale_color_manual(values=c('#5555EE', '#FFA0A0', '#50FF50', '#363636')) +
#       annotate(
#         'text',
#         label=paste('umbrae (binomial com baseline normal): ', round(umbrae_res, 5),
#                     '\n MAD/mean binomial (*):', ifelse(is.numeric(mad.mean.daily.nloptr),
#                                                     round(mad.mean.daily.nloptr, 5), NA),
#                     '\n MAD/mean normal (**):', ifelse(is.numeric(mad.mean.daily.nls),
#                                                   round(mad.mean.daily.nls, 5), NA),
#                     '\n * / **: ', round(mad.mean.daily.nloptr / mad.mean.daily.nls, 5)),
#         x=as.Date('2023-02-01'),
#         y=sum(y)*0.2
#       ) +
#       annotate(
#         'text',
#         label=paste('AIC binomial:', ifelse(is.numeric(aic.nloptr),
#                                                         round(aic.nloptr, 5), NA),
#                     '\n AIC normal:', ifelse(is.numeric(aic.nls),
#                                                        round(aic.nls, 5), NA),
#                     '\n BIC binomial:', ifelse(is.numeric(bic.nloptr),
#                                             round(bic.nloptr, 5), NA),
#                     '\n BIC normal:', ifelse(is.numeric(bic.nls),
#                                              round(bic.nls, 5), NA)
#                     ),
#         x=as.Date('2020-09-01'),
#         y=sum(y)*0.8
#       ) +
#       ggtitle(paste(method, 'com parametro', p))
#     plot
#   }
#   return(fn)
# }
# 
# gera_grafico.grupo <- function(cen, method) {
#   date_ <- cen[1, 'date'][[1]][[1]]
#   y <- cen[1, 'y_diff'][[1]][[1]]
#   cen <- cen %>% mutate(
#     y_hat=ifelse(erro, -1, y_hat),
#     chute_inicial=ifelse(erro, 0, chute_inicial)
#   )
#   
#   plots <- lapply(unique(cen$param), gera_grafico.fn(cen, date_, y, 'method'))
#   plot <- ggarrange(plotlist=plots, ncol=2, common.legend = T, nrow = 2)
#   plot
# }
# 
# gera_pagina.fn <- function(cenario, method) {
#   fn <- function(linha) {
#     cenario_total <- cenario[cenario$administrative_area_level_2 == linha$administrative_area_level_2 &
#                              cenario$administrative_area_level_3 == linha$administrative_area_level_3,]
#     cen <- cenario_total[cenario_total$weekly, ]
#     weekly <- gera_grafico.grupo(cen, method)
#     
#     cen <- cenario_total[!cenario_total$weekly, ]
#     n_weekly <- gera_grafico.grupo(cen, method)
#     
#     fig <- ggarrange(weekly, n_weekly, nrow=2, common.legend = T, labels = c('Semanal', 'Diário'))
#     annotate_figure(fig, top = text_grob(paste(linha$administrative_area_level_2,
#                                                linha$administrative_area_level_3,
#                                                'População',
#                                                linha$pop,
#                                                sep = '-'),
#                                          face = "bold", size = 14))
#   }
# }
# 
# plots <- by(locais,
#             seq_len(nrow(locais)),
#             gera_pagina.fn(cenarios_splines, 'Splines'))
# 
# 
# pdf(paste('./splines__.pdf', sep=''),
#     width=18, height=12)
# plots
# dev.off()
# 
# 
# cenarios_kriston <- readRDS('./cenarios_amostrados_kriston.rds')
# cenarios_kriston$erro <- is.na(cenarios_kriston$mad.mean.cummulative)
# plots <- by(locais,
#             seq_len(nrow(locais)),
#             gera_pagina.fn(cenarios_kriston, 'Kriston'))
# 
# pdf(paste('./kriston_.pdf', sep=''),
#     width=18, height=12)
# plots
# dev.off()

# Kriston 4 semanas
# Splines: Cidades até pequenas: 7 gl diario
# Outras: 25gl diario
# Só binomial

locais <- readRDS('./locais.rds')
hiperparametros <- tibble(weekly=c(T, F),
                                  method=c('kriston_method', 'splines_method'),
                                  param=c(4, 25))

cenarios <- cross_join(locais, hiperparametros)
cenarios[cenarios$categoria %in% c('muito pequena',
                                   'pequena') &
           cenarios$method == 'splines_method',
        'param'] <- 7

cenarios <- cenarios %>% 
  mutate(fit_tendencia=pmap(
    list(y, pop, param, method, date, weekly),
    function(y, pop, param, method, date, weekly) {
      tryCatch(quietly(estima_tendencia)(y,
                                         pop,
                                         params=T, 
                                         param=param, 
                                         wcmethod=method,
                                         dates = date,
                                         weekly=weekly, 
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
cenarios$loglik <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$loglik,
                              error=function(err) as.character(err)))
cenarios$aic <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$aic,
                              error=function(err) as.character(err)))
cenarios$bic <- cenarios$fit_tendencia %>% 
  lapply(function(t) tryCatch(t$bic,
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
saveRDS(cenarios, './cenarios_amostrados_compara_kriston_splines.rds')
rm(cenarios)
rm(x)

# Rodar a partir daqui

cenarios <- readRDS('cenarios_amostrados_compara_kriston_splines.rds')
cenarios$param <- unlist(as.factor(cenarios$param))
cenarios$mad.mean.daily <- as.numeric(cenarios$mad.mean.daily)

cenarios %>% filter(!erro) %>%
  ggplot(aes(x=method, y=mad.mean.daily)) +
  geom_boxplot() +
  stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9)


cenarios$n_ondas <- 0
cenarios[!cenarios$erro,]$n_ondas <- cenarios[!cenarios$erro,]$fit_tendencia %>%
  lapply(FUN=function(fit) {
    tryCatch({
        return(length(fit$params) / 3)
      },
     error=function(err) {
       return(fit$params)
     }
    )
  })

cenarios_kriston <- cenarios %>% filter(method == 'kriston_method')
cenarios_splines <- cenarios %>% filter(method == 'splines_method')

cenarios_gpd <- merge(
  cenarios_kriston[, c('categoria',
                       'administrative_area_level_2',
                       'administrative_area_level_3',
                       'bic',
                       'aic',
                       'loglik',
                       'mad.mean.daily',
                       'erro',
                       'n_ondas',
                       'y_diff',
                       'y_hat',
                       'population',
                       'date')],
  cenarios_splines[, c('administrative_area_level_2',
                       'administrative_area_level_3',
                       'bic',
                       'aic',
                       'loglik',
                       'mad.mean.daily',
                       'erro',
                       'n_ondas',
                       'y_hat')],
  by=c('administrative_area_level_2', 'administrative_area_level_3')
)

(unlist(cenarios_gpd$n_ondas.x) - unlist(cenarios_gpd$n_ondas.y)) %>% hist()

cenarios_gpd <- cenarios_gpd %>% 
  filter(!erro.x) %>% 
  mutate(
    bic.x=as.numeric(bic.x),
    bic.y=as.numeric(bic.y),
    aic.x=as.numeric(aic.x),
    aic.y=as.numeric(aic.y),
    bic.diff=(bic.x - bic.y) / pmax(bic.x, bic.y),
    aic.diff=(aic.x - aic.y) / pmax(aic.x, aic.y),
  )

cenarios_gpd %>% 
  ggplot(aes(y=aic.diff)) +
  geom_boxplot() +
  facet_grid(.~categoria)

cenarios_gpd %>% 
  ggplot(aes(y=bic.diff)) +
  geom_boxplot() +
  facet_grid(.~categoria)

cenarios_gpd <- cenarios_gpd %>% 
  mutate(
    lambda=-2 *(as.numeric(loglik.x) - as.numeric(loglik.y)),
    df=3 * (as.numeric(n_ondas.y) - as.numeric(n_ondas.x))
  )
cenarios_gpd <- cenarios_gpd %>%
  mutate(
    h0=if_else(lambda > 0,
               if_else(df > 0,
                      'Kriston = Splines \n contra Splines > Kriston',
                      'Modelo com mais parâmetros tem verossimilhança menor'),
               if_else(df < 0,
                       'Splines = Kriston \n contra Kriston > Splines',
                       'Modelo com mais parâmetros tem verossimilhança menor')
               ),
    lambda=abs(lambda),
    df=abs(df)
  )

cenarios_gpd %>% filter(df != 0) %>%  ggplot(aes(h0)) + geom_bar()


cenarios_gpd <- cenarios_gpd %>% 
  mutate(
    p=pchisq(lambda, df, lower.tail = F),
    alpha=0.05,
    significante=p<alpha
  )
# Turuçu: Umbrae indica que Spline é melhor, mas não rejeitamos H0
cenarios_gpd[cenarios_gpd$h0 != 'Modelo com mais parâmetros tem verossimilhança menor',]$significante %>% 
  mean()

cenarios$n_ondas %>% as.numeric() %>%  hist()
cenarios$n_ondas %>% as.numeric() %>% summary()
counts <- cenarios %>% 
  group_by(categoria, method, n_ondas) %>% 
  summarise(count=n())

medias <- counts %>%
  group_by(categoria, method) %>% 
  summarise(media=weighted.mean(as.numeric(n_ondas), count),
            mediana=median(rep(as.numeric(n_ondas), times=count))
  )

counts %>% 
  ggplot(aes(x=factor(as.numeric(n_ondas)), y=count)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=count), vjust=-0.25) +
  geom_vline(data=medias, aes(xintercept = media + 1, colour = 'Média')) +
  geom_vline(data=medias,
             aes(xintercept = mediana + 1, colour='Mediana'),
             linetype = 'dashed') +
  facet_grid(method~categoria)

gera_grafico.fn <- function(cen.param, date_, y, plot_name) {
  fn <- function() {
    cen.param <- cen.param %>% mutate(
      mad.mean.daily.x=ifelse(erro.x, Inf, mad.mean.daily.x),
      mad.mean.daily.y=ifelse(erro.y, Inf, mad.mean.daily.y),
    )
    if (nrow(cen.param[!cen.param$erro.x]) == 0) {
      df <- data.frame('Real'=cumsum(y),
                       'Dia'=date_[2:length(date_)])
      plot <- ggplot(df) +
        geom_point(aes(x=Dia, y=Real, color='Real'), alpha=0.3) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = 'none') +
        scale_color_manual(values=c('#363636')) +
        ggtitle(plot_name)
      return(plot)
    }
    y_hat.kriston <- cen.param[1, 'y_hat.x'][[1]][[1]]
    y_hat.splines <- cen.param[1, 'y_hat.y'][[1]][[1]]
    
    mad.mean.daily.kriston <- cen.param[1, 'mad.mean.daily.x'][[1]][[1]]
    mad.mean.daily.splines <- cen.param[1, 'mad.mean.daily.y'][[1]][[1]]
    
    aic.kriston <- cen.param[1, 'aic.x'][[1]][[1]]
    aic.splines <- cen.param[1, 'aic.y'][[1]][[1]]
    
    bic.kriston <- cen.param[1, 'bic.x'][[1]][[1]]
    bic.splines <- cen.param[1, 'bic.y'][[1]][[1]]
    bic.splines <- cen.param[1, 'bic.y'][[1]][[1]]
    h0 <- cen.param[1, 'h0'][[1]][[1]]
    df <- cen.param[1, 'df'][[1]][[1]]
    lambda <- cen.param[1, 'lambda'][[1]][[1]]
    
    umbrae_res <- umbrae(y, y_hat.kriston, y_hat.splines)
    data <- data.frame('Real'=cumsum(y),
                     'Kriston'=cumsum(y_hat.kriston),
                     'Splines'=cumsum(y_hat.splines),
                     'Dia'=date_[2:length(date_)])
    plot <- ggplot(data) +
      geom_point(aes(x=Dia, y=Real, color='Real'), alpha=0.3) +
      geom_line(aes(x=Dia, y=Kriston, color='Kriston')) +
      geom_line(aes(x=Dia, y=Splines, color='Splines')) +
      # theme(
      #   axis.title.x = element_blank(),
      #   axis.text.x = element_blank(),
      #   axis.ticks.x=element_blank(),
      #   legend.position = 'none') +
      scale_color_manual(values=c('#FFA0A0', '#363636', '#5555EE', '#50FF50')) +
      annotate(
        'text',
        label=paste('umbrae (kriston com baseline splines): ', round(umbrae_res, 5),
                    '\n MAD/mean kriston (*):', ifelse(is.numeric(mad.mean.daily.kriston),
                                                       round(mad.mean.daily.kriston, 5), NA),
                    '\n MAD/mean splines (**):', ifelse(is.numeric(mad.mean.daily.splines),
                                                        round(mad.mean.daily.splines, 5), NA),
                    '\n * / **: ', round(mad.mean.daily.kriston / mad.mean.daily.splines, 5)),
        x=as.Date('2023-02-01'),
        y=sum(y)*0.2
      ) +
      annotate(
        'text',
        label=paste('AIC kriston:', ifelse(is.numeric(aic.kriston),
                                           round(aic.kriston, 5), NA),
                    '\n AIC splines:', ifelse(is.numeric(aic.splines),
                                              round(aic.splines, 5), NA),
                    '\n BIC kriston:', ifelse(is.numeric(bic.kriston),
                                              round(bic.kriston, 5), NA),
                    '\n BIC splines:', ifelse(is.numeric(bic.splines),
                                              round(bic.splines, 5), NA),
                    '\n \n H0:', h0,
                    '\n df:', df,
                    '\n Lambda:', lambda,
                    '\n p-value:', pchisq(lambda, df, lower.tail = F)
        ),
        x=as.Date('2020-09-01'),
        y=sum(y)*0.8
      )+
      ggtitle(plot_name)
    plot
  }
  return(fn())
}

gera_pagina.fn <- function(cenario) {
  fn <- function(linha) {
    cen <- cenario[cenario$administrative_area_level_2 == linha$administrative_area_level_2 &
                     cenario$administrative_area_level_3 == linha$administrative_area_level_3,]
    date_ <- cen[1, 'date'][[1]][[1]]
    y <- cen[1, 'y_diff'][[1]][[1]]
    plot <- gera_grafico.fn(cen, date_, y, paste(linha$administrative_area_level_2,
                                                 linha$administrative_area_level_3,
                                                 cen$population))
    plot
  }
}
cenarios_gpd <- cenarios_gpd %>% arrange(population)
gera_pagina <- gera_pagina.fn(cenarios_gpd)
plots <- by(cenarios_gpd,
            seq_len(nrow(cenarios_gpd)),
            gera_pagina)
plots[[1]]
pdf(paste('./compara.pdf', sep=''),
    width=18, height=12)
plots
dev.off()
