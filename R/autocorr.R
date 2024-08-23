library(dplyr)
library(forecast)
library(ggpubr)
library(car)
library(purrr)
library(ggdendro)
library("ape")
library(tidyr)
library(plotly)
library(htmlwidgets)

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
    return(test)
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
    test
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

# Dendograma
autocor <- cenarios_kriston$residuo %>% lapply(FUN=function(res) (res %>% acf(plot = F, lag.max = 365))$acf)
autocor_df <- as.data.frame(do.call(rbind, autocor))
colnames(autocor_df) <- 1:366
autocor_df$cidade <- cenarios_kriston$administrative_area_level_3
autocor_df$estado <- cenarios_kriston$administrative_area_level_2
autocor_df$categoria <- cenarios_kriston$categoria

distancias <- autocor_df[, -c(52, 53, 54)] %>% dist(diag=T)
hc <- hclust(distancias, method='ward.D')
k <- 30
colors = rainbow(k)
groups <- cutree(hc, k=k)
plot(as.phylo(hc), tip.color = colors[groups], type='tidy', direction='downwards',
     label.offset = 0, cex = 0.7, no.margin = TRUE)

plot(as.phylo(hc), tip.color = colors[cenarios_kriston$categoria], type='tidy', direction='downwards',
     label.offset = 0, cex = 0.7, no.margin = TRUE)

cenarios_kriston$grupo <- factor(groups)
cenarios_kriston %>% ggplot(aes(x=grupo, y=population)) +
  geom_boxplot()

autocor_df$grupo <- factor(groups)
autocor_df_longo <- autocor_df %>% pivot_longer(cols=`2`:`366`,
                            names_to = 'lag',
                            values_to = 'autocorrelacao')

autocor_df_longo$lag <- autocor_df_longo$lag %>% as.integer()
lags <- unique(autocor_df_longo$lag)
pdf('kriston_acf_agrupados.pdf',
    width=18, height=12)
for (var in unique(autocor_df_longo$grupo)) {
  grafico <- autocor_df_longo %>% 
    filter(grupo == var) %>% 
    ggplot(aes(x=as.factor(lag), y=autocorrelacao)) +
    # geom_boxplot(alpha=0.2) +
    geom_line(aes(color=cidade, group = cidade), alpha=0.5) +
    geom_point(aes(color=cidade), alpha=0.5, size=2) +
    stat_summary(fun=mean, color='#EEAA00', size=3) +
    geom_abline(slope=0, intercept=0) +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks=lags[(2:366 -1) %% 7 == 0]) +
    theme(legend.position="bottom")
  saveWidget(grafico %>% ggplotly(),
             paste('kriston_grupo_', var, '.html', sep=''))
  # print(grafico)
}
dev.off()



autocor <- cenarios_splines$residuo %>% lapply(FUN=function(res) (res %>% acf(plot = F, lag.max = 365))$acf)
autocor_df <- as.data.frame(do.call(rbind, autocor))
colnames(autocor_df) <- 1:366
autocor_df$cidade <- cenarios_splines$administrative_area_level_3
autocor_df$estado <- cenarios_splines$administrative_area_level_2
autocor_df$categoria <- cenarios_splines$categoria

distancias <- autocor_df[, -c(52, 53, 54)] %>% dist(diag=T)
hc <- hclust(distancias, method='ward.D')
k <- 30
colors = rainbow(k)
groups <- cutree(hc, k=k)
plot(as.phylo(hc), tip.color = colors[groups], type='tidy', direction='downwards',
     label.offset = 0, cex = 0.7, no.margin = TRUE)

plot(as.phylo(hc), tip.color = colors[cenarios_splines$categoria], type='tidy', direction='downwards',
     label.offset = 0, cex = 0.7, no.margin = TRUE)

cenarios_splines$grupo <- factor(groups)
cenarios_splines %>% ggplot(aes(x=grupo, y=population)) +
  geom_boxplot()

autocor_df$grupo <- factor(groups)
autocor_df_longo <- autocor_df %>% pivot_longer(cols=`2`:`366`,
                                                names_to = 'lag',
                                                values_to = 'autocorrelacao')

autocor_df_longo$lag <- autocor_df_longo$lag %>% as.integer()
lags <- unique(autocor_df_longo$lag)
pdf('splines_acf_agrupados.pdf',
    width=18, height=12)
for (var in unique(autocor_df_longo$grupo)) {
  grafico <- autocor_df_longo %>% 
    filter(grupo == var) %>% 
    ggplot(aes(x=as.factor(lag), y=autocorrelacao)) +
    # geom_boxplot(alpha=0.2) +
    geom_line(aes(color=cidade, group = cidade), alpha=0.5) +
    geom_point(aes(color=cidade), alpha=0.5) +
    stat_summary(fun=mean, color='#EEAA00') +
    geom_abline(slope=0, intercept=0) +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks=lags[(2:366 -1) %% 7 == 0]) +
    theme(legend.position="bottom")
  # saveWidget(grafico %>% ggplotly(),
  #            paste('splines_grupo_', var, '.html', sep=''))
  print(grafico)
}
dev.off()
