source("./src/services/estabiliza_serie.R")

grafico_modelo <- function(serie, datas, tipo_modelo, titulo_grafico, eixo_x, eixo_y, w=90) {
  serie_padronizada <- estabiliza_serie(serie, width=w)
  dados = tsibble(
    data = datas[w:length(datas)],
    y = serie_padronizada,
    index = data
  )
  if (tipo_modelo == 'Sugerido') {
    p <- calcula_p(dados)
    q <- calcula_q(dados)
    modelo <- dados %>% model(model=ARIMA(y ~ 1 + pdq(p, 0, q) + PDQ(0, 0, 0)))
  } else {
    modelo <- dados %>% model(model=ARIMA(y, stepwise=tipo_modelo=="Busca stepwise"))
    p <- modelo$model[[1]]$fit$spec$p
    q <- modelo$model[[1]]$fit$spec$q
  }
  
  if (!is.numeric(p) || !is.numeric(q)) {
    stop("Error: p or q is not numeric.")
  }

  G <- modelo %>% forecast(h=30) %>% autoplot(dados) +
    labs(
      x = eixo_x,
      y = eixo_y,
      title = titulo_grafico
    ) +
    annotate("text", x=min(dados$data)+30, y=max(dados$y)-0.2, label=paste0("p = ",p,"\nq = ",q,"\nAIC = ",round(modelo$model[[1]]$fit$fit$AICc,2))) +
    theme_minimal() 
  return(G)
}

render_grafico_modelo <- function(input) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var
  tipo_modelo <- input$tipo_modelo
  
  df <- carregar_dados(est, cid, slider, variavel, TRUE)
  
  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  if(variavel == "vaccines"){
    escala <- 10000
  } else {
    escala <- 1
  }
  return(grafico_modelo(df[[variavel]] / escala, df$date, tipo_modelo, 'Valor real e previsões', "Data", "Modelo para a série estacionária"))
}


grafico_residuo <- function(serie, datas, tipo_modelo, w=90) {
  serie_padronizada <- estabiliza_serie(serie, width=w)
  dados = tsibble(
    data = datas[w:length(datas)],
    y = serie_padronizada,
    index = data
  )
  if (tipo_modelo == 'Sugerido') {
    p <- calcula_p(dados)
    q <- calcula_q(dados)
    modelo <- dados %>% model(model=ARIMA(y ~ 1 + pdq(p, 0, q) + PDQ(0, 0, 0)))
  } else {
    modelo <- dados %>% model(model=ARIMA(y, stepwise=tipo_modelo=="Busca stepwise"))
  }
  G <- modelo %>% gg_tsresiduals()
  return(G)
}

render_grafico_residuo <- function(input) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var
  tipo_modelo <- input$tipo_modelo
  
  df <- carregar_dados(est, cid, slider, variavel, TRUE)
  
  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  if(variavel == "vaccines"){
    escala <- 10000
  } else {
    escala <- 1
  }
  return(grafico_residuo(df[[variavel]] / escala, df$date, tipo_modelo))
}


grafico_modelo_inv_transf <- function(series, datas, tipo_modelo, titulo_grafico, eixo_x, eixo_y, w=90){
  serie_padronizada <- estabiliza_serie(series, width=w, invertible=T)
  n <- length(series)
  serie <- serie_padronizada$serie
  sd <- serie_padronizada$sd
  params <- serie_padronizada$params
  dados = tsibble(
    data = datas[w:length(datas)],
    y = serie,
    index = data
  )
  if (tipo_modelo == 'Sugerido') {
    p <- calcula_p(dados)
    q <- calcula_q(dados)
    modelo <- dados %>% model(model=ARIMA(y ~ 1 + pdq(p, 0, q) + PDQ(0, 0, 0)))
    pred <- modelo %>% forecast(h=30)
    prevs <- (pred$.mean %>% diffinv(lag=7, xi=serie[(length(serie)-6):length(serie)]) * sd[length(sd)])[8:37] + diff(calcula_tend(params, (n):(n+30))$n)
    
  } else {
    modelo <- dados %>% model(model=ARIMA(y, stepwise=tipo_modelo=="Busca stepwise"))
    pred <- modelo %>% forecast(h=30)
    prevs <- (pred$.mean %>% diffinv(lag=7, xi=serie[(length(serie)-6):length(serie)]) * sd[length(sd)])[8:37] + diff(calcula_tend(params, (n):(n+30))$n)
  }
  
  sigma <- parameters(pred$y)$sigma * sd[length(sd)]
  
  ic_80 <- data.frame(lower=qnorm(0.1, prevs, sigma), upper=qnorm(0.9, prevs, sigma))
  ic_95 <- data.frame(lower=qnorm(0.025, prevs, sigma), upper=qnorm(0.975, prevs, sigma))
  df_novo <- data.frame(
    value=c(diff(series), prevs),
    t <- 2:(n+30),
    lower_80=c(diff(series), ic_80$lower),
    upper_80=c(diff(series), ic_80$upper),
    lower_95=c(diff(series), ic_95$lower),
    upper_95=c(diff(series), ic_95$upper)
  )
  
  G <- df_novo %>%
    ggplot(aes(x=t, y=value)) +
    geom_line() +
    geom_linerange(aes(ymin=lower_80, ymax=upper_80, nivel='80%'), color='blue', alpha=0.5) +
    geom_linerange(aes(ymin=lower_95, ymax=upper_95, nivel='95%'), color='blue', alpha=0.20) +
    theme_minimal() +
    labs(
      x = eixo_x,
      y = eixo_y,
      title = titulo_grafico
    ) +
    theme(
      strip.background = element_rect(fill='lightgray', colour='lightgray')
    )
  
  return(G)
}


render_grafico_modelo_inv_transf <- function(input) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var
  tipo_modelo <- input$tipo_modelo
  
  df <- carregar_dados(est, cid, slider, variavel, TRUE)
  
  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  if(variavel == "vaccines"){
    escala <- 10000
  } else {
    escala <- 1
  }
  return(grafico_modelo_inv_transf(df[[variavel]] / escala, df$date, tipo_modelo, 'Valor real e previsões', "Data", "Modelo para a série"))
}

# 
# df <- carregar_dados('', '', c('2020-01-01', '2023-06-01'), 'deaths')
# 
# w <- 90
# serie_padronizada <- estabiliza_serie(df$deaths, width=w, invertible=T)
# dados = tsibble(
#   data = df$date[w:length(df$date)],
#   y = serie_padronizada$serie,
#   index = data
# )
# 
# serie <- serie_padronizada$serie
# sd <- serie_padronizada$sd
# params <- serie_padronizada$params
# 
# p <- calcula_p(dados)
# q <- calcula_q(dados)
# cat(p, q)
# modelo <- dados %>% model(model=ARIMA(y ~ 1 + pdq(p, 0, q) + PDQ(0, 0, 0)))
# modelo %>% gg_tsresiduals()
# modelo %>% forecast(h=30) %>% autoplot(dados) +
#   theme_minimal()
# 
# modelo %>% forecast(h=30) %>% autoplot(dados) +
#   annotate("text", x=min(dados$data)+30, y=max(dados$y)-0.2, label=paste0("p = ",p,"\nq = ",q,"\nAIC = ",round(modelo$model[[1]]$fit$fit$AICc,2))) +
#   theme_minimal()
# 
# pred <- modelo %>% forecast(h=30)
# n <- length(df$deaths)
# prevs <- (pred$.mean %>% diffinv(lag=7, xi=serie[(length(serie)-6):length(serie)]) * sd[length(sd)])[8:37] + diff(calcula_tend(params, (n):(n+30))$n)
# prevs
# pred
# 
# sigma <- parameters(pred$y)$sigma * sd[length(sd)]
# 
# ic_80 <- data.frame(lower=qnorm(0.1, prevs, sigma), upper=qnorm(0.9, prevs, sigma))
# ic_95 <- data.frame(lower=qnorm(0.025, prevs, sigma), upper=qnorm(0.975, prevs, sigma))
# df_novo <- data.frame(
#   value=c(diff(df$deaths), prevs),
#   t <- 2:(n+30),
#   lower_80=c(diff(df$deaths), ic_80$lower),
#   upper_80=c(diff(df$deaths), ic_80$upper),
#   lower_95=c(diff(df$deaths), ic_95$lower),
#   upper_95=c(diff(df$deaths), ic_95$upper)
# )
# 
# df_novo[(n-1):(n+29),]
# 
# df_novo %>%
#   ggplot(aes(x=t, y=value)) +
#   geom_line() +
#   geom_linerange(aes(ymin=lower_80, ymax=upper_80), color='blue', alpha=0.5) +
#   geom_linerange(aes(ymin=lower_95, ymax=upper_95), color='blue', alpha=0.20) +
#   theme_minimal() +
#   theme(
#     strip.background = element_rect(fill='lightgray', colour='lightgray')
#   )
