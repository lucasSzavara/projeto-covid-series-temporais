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
  mutate(residuo=map(y_diff, 
                  function(y) {
                    tryCatch({
                      y_diario <- y %>% log1p()
                      y_diff <- diff(y_diario)
                      y_diff <- diff(y_diff, lag = 7)
                      y_diff
                      },
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
autocor <- cenarios_kriston$residuo %>% lapply(FUN=function(res) (res %>% acf(plot = F, lag.max = 50))$acf)
autocor_df <- as.data.frame(do.call(rbind, autocor))
autocorp <- cenarios_kriston$residuo %>% lapply(FUN=function(res) (res %>% pacf(plot = F, lag.max = 50))$acf)
autocorp_df <- as.data.frame(do.call(rbind, autocorp))
colnames(autocor_df) <- 0:50
colnames(autocorp_df) <-  paste('p', 1:50)
autocor_df$tipo <- 'ACF'
autocor_df$cidade <- cenarios_kriston$administrative_area_level_3
autocor_df$estado <- cenarios_kriston$administrative_area_level_2
autocor_df$categoria <- cenarios_kriston$categoria
autocor_df <- autocor_df[, -c(1)]

distancias <- cbind(autocor_df[, -c(52, 53, 54, 51)], autocorp_df) %>%
  dist(diag=T)
autocorp_df$tipo <- 'PACF'
autocorp_df$cidade <- cenarios_kriston$administrative_area_level_3
autocorp_df$estado <- cenarios_kriston$administrative_area_level_2
autocorp_df$categoria <- cenarios_kriston$categoria
hc <- hclust(distancias, method='ward.D')
k <- 6
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
autocorp_df$grupo <- factor(groups)
colnames(autocorp_df) <-  c(1:50, 'tipo', 'cidade', 'estado', 'categoria', 'grupo')
autocor_t_df <- rbind(autocor_df, autocorp_df)
autocor_df_longo <- autocor_t_df %>% pivot_longer(cols=`1`:`50`,
                            names_to = 'lag',
                            values_to = 'autocorrelacao')

autocor_df_longo$lag <- autocor_df_longo$lag %>% as.integer()
lags <- unique(autocor_df_longo$lag)
pdf('acf_agrupados.pdf',
    width=18, height=12)
for (var in unique(autocor_df_longo$grupo)) {
  df.cidades <- autocor_df_longo%>% 
    filter(grupo == var)
  cidades <- df.cidades[, c('cidade', 'estado')] %>% unique()
  n_por_cidades <- cidades %>% 
    apply(1, FUN=function(cid) {
      (cenarios_kriston %>%
        filter((administrative_area_level_3 == cid[1]) &
                 (administrative_area_level_2 == cid[2])))$residuo[[1]] %>% 
        length()
    })
  n_max <- n_por_cidades %>% max()
  n_media <- n_por_cidades %>% sum()
  n_cidades <- cidades$cidade %>% 
    length()
  grafico_acf <- df.cidades %>%
    filter(tipo == 'ACF') %>% 
    ggplot(aes(x=as.factor(lag), y=autocorrelacao)) +
    # geom_boxplot(alpha=0.2) +
    geom_point(aes(color=cidade), alpha=0.2, size=2) +
    stat_summary(fun=mean, color='#EEAA00', size=1) +
    geom_abline(slope=0, intercept=0) +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks=lags[(2:51 -1) %% 7 == 0]) +
    geom_abline(slope=0, intercept=qnorm(0.025) / sqrt(n_max), linetype = 'dashed') +
    geom_abline(slope=0, intercept=-qnorm(0.025) / sqrt(n_max), linetype = 'dashed') +
    geom_abline(slope=0, intercept=qnorm(0.025) / sqrt(n_media), linetype = 'dotted') +
    geom_abline(slope=0, intercept=-qnorm(0.025) / sqrt(n_media), linetype = 'dotted') +
    theme(legend.position="none") +
    ylab('ACF')
  grafico_pacf <- df.cidades %>%
    filter(tipo == 'PACF') %>% 
    ggplot(aes(x=as.factor(lag), y=autocorrelacao)) +
    # geom_boxplot(alpha=0.2) +
    geom_point(aes(color=cidade), alpha=0.2, size=2) +
    stat_summary(fun=mean, color='#EEAA00', size=1) +
    geom_abline(slope=0, intercept=0) +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks=lags[(2:51 -1) %% 7 == 0]) +
    geom_abline(slope=0, intercept=qnorm(0.025) / sqrt(n_max), linetype = 'dashed') +
    geom_abline(slope=0, intercept=-qnorm(0.025) / sqrt(n_max), linetype = 'dashed') +
    geom_abline(slope=0, intercept=qnorm(0.025) / sqrt(n_media), linetype = 'dotted') +
    geom_abline(slope=0, intercept=-qnorm(0.025) / sqrt(n_media), linetype = 'dotted') +
    theme(legend.position="none") +
    ylab('PACF')
  grafico <- ggarrange(grafico_acf, grafico_pacf, legend='bottom', common.legend = T)
  # saveWidget(grafico %>% ggplotly(),
  #            paste('grupo_', var, '.html', sep=''))
  print(grafico)
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

