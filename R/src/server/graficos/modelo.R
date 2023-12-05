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
  }
  G <- modelo %>% forecast(h=30) %>% autoplot(dados) +
    labs(
      x = eixo_x,
      y = eixo_y,
      title = titulo_grafico
    ) +
    theme_minimal()
  return(G)
}

render_grafico_modelo <- function(input) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var
  tipo_modelo <- input$tipo_modelo
  
  df <- carregar_dados(est, cid, slider, variavel)
  
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
  
  df <- carregar_dados(est, cid, slider, variavel)
  
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

# 
# df <- carregar_dados('', '', c('2020-01-01', '2023-06-01'), 'deaths')
# 
# w <- 90
# serie_padronizada <- estabiliza_serie(df$deaths, width=w)
# dados = tsibble(
#   data = df$date[w:length(df$date)],
#   y = serie_padronizada,
#   index = data
# )
# 
# p <- calcula_p(dados)
# q <- calcula_q(dados)
# cat(p, q)
# modelo <- dados %>% model(model=ARIMA(y ~ 1 + pdq(p, 0, q) + PDQ(0, 0, 0)))
# modelo %>% gg_tsresiduals()
# modelo %>% forecast(h=30) %>% autoplot(dados) +
#   theme_minimal()
