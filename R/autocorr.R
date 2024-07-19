library(dplyr)
library(forecast)
library(ggpubr)
library(car)

cenarios <- readRDS('cenarios_amostrados_compara_kriston_splines.rds')

cenarios <- cenarios %>% 
  filter(!erro) 
cenarios <- cenarios %>% 
  mutate(residuo=map2(y_diff, y_hat, 
                  function(y, y_hat) {
                    tryCatch(
                      y_hat - y,
                      error=function(err) as.character(err)
                    )
                  }, .progress = list(
                    type = "iterator", 
                    clear = T,
                    show_after=0
                  )
  )
  )

cenarios_kriston <- cenarios %>% filter(method == 'kriston_method')
cenarios_splines <- cenarios %>% filter(method == 'splines_method')

plota_acf <- function(linha) {
  test <- Box.test(linha$residuo[[1]],
                   type = "Ljung-Box",
                   fitdf = 0,
                   lag=15)
  valor.p <- test$p.value
  acf <- ggAcf(linha$residuo[[1]], lag.max=50)
  pacf <- ggPacf(linha$residuo[[1]], lag.max=50)
  fig <- ggarrange(acf,
                   pacf,
                   nrow=1,
                   ncol=2,
                   common.legend = T)
  annotate_figure(fig, top = text_grob(paste(linha$administrative_area_level_2,
                                             linha$administrative_area_level_3,
                                             'População',
                                             linha$pop,
                                             'Valor-p:',
                                             valor.p,
                                             sep = '-'),
                                       face = "bold", size = 14))
}

# mod <- lm(cenarios_kriston[1, 'y_diff'][[1]][[1]] ~ cenarios_kriston[1, 'y_hat'][[1]][[1]])
# durbinWatsonTest(mod)

plots <- by(cenarios_kriston,
            seq_len(nrow(cenarios_kriston)),
            plota_acf)
plots[[1]]
pdf(paste('./acf_kriston.pdf', sep=''),
    width=18, height=12)
plots
dev.off()


plots <- by(cenarios_splines,
            seq_len(nrow(cenarios_splines)),
            plota_acf)

pdf(paste('./acf_splines.pdf', sep=''),
    width=18, height=12)
plots
dev.off()

cenarios_kriston$teste <- cenarios_kriston$residuo %>% 
  lapply(function(x) {
    test <- Box.test(x,
                     type = "Ljung-Box",
                     fitdf = 0,
                     lag=15)
  })

cenarios_kriston$p.value <- unlist(cenarios_kriston$teste %>% 
  lapply(function(x) {
    x$p.value
  }))
cenarios_kriston$autocorr <- cenarios_kriston$p.value < 0.05

cenarios_kriston %>%
  ggplot(aes(x=p.value)) +
  geom_histogram() +
  geom_vline(aes(xintercept = 0.05)) 
cenarios_kriston$autocorr %>% mean()



cenarios_splines$teste <- cenarios_splines$residuo %>% 
  lapply(function(x) {
    test <- Box.test(x,
                     type = "Ljung-Box",
                     fitdf = 0,
                     lag=15)
  })

cenarios_splines$p.value <- unlist(cenarios_splines$teste %>% 
                                     lapply(function(x) {  
                                       x$p.value
                                     }))
cenarios_splines$autocorr <- cenarios_splines$p.value < 0.05

cenarios_splines %>%
  ggplot(aes(x=p.value)) +
  geom_histogram() +
  geom_vline(aes(xintercept = 0.05)) 
cenarios_splines$autocorr %>% mean()
