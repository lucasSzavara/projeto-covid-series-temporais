source("./src/services/estabiliza_serie.R")

grafico_modelo <- function(serie, datas, titulo_grafico, eixo_x, eixo_y) {
  serie_padronizada <- estabiliza_serie(serie)
  dados = tsibble(
    data = datas,
    y = serie_padronizada,
    index = data
  )
  modelo <- dados %>% model(model=ARIMA(y, stepwise=FALSE))
  G <- modelo %>% forecast(h=30) %>% autoplot(dados) +
    labs(
      x = eixo_x,
      y = eixo_y,
      title = titulo_grafico
    ) +
    theme_minimal()
  fig <- ggplotly(G + theme(plot.title=element_text(size=10)))
  return(fig)
}

render_grafico_modelo <- function(input) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var
  
  df <- carregar_dados(est, cid, slider, variavel)
  
  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  titulo <- titulo_series_tendencia(variavel, est, cid)
  
  if(variavel == "vaccines"){
    escala <- 10000
  } else {
    escala <- 1
  }
  return(grafico_modelo(df[[variavel]] / escala, df$date, titulo, "Data", "Novos Confirmados Diários"))
}


grafico_residuo <- function(serie, datas, titulo_grafico, eixo_x, eixo_y) {
  serie_padronizada <- estabiliza_serie(serie)
  dados = tsibble(
    data = datas,
    y = serie_padronizada,
    index = data
  )
  modelo <- dados %>% model(model=ARIMA(y, stepwise=FALSE))
  G <- modelo %>% gg_tsresiduals()
  fig <- ggplotly(G)
  return(fig)
}

render_grafico_residuo <- function(input) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var
  
  df <- carregar_dados(est, cid, slider, variavel)
  
  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  titulo <- titulo_series_tendencia(variavel, est, cid)
  
  if(variavel == "vaccines"){
    escala <- 10000
  } else {
    escala <- 1
  }
  return(grafico_residuo(df[[variavel]] / escala, df$date, titulo, "Data", "Novos Confirmados Diários"))
}