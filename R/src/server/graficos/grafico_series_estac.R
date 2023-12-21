# Funçao geral para o grafico de series estacionaria

#-----------------------------------------------------------------------------------------------

# Funçao que gera o grafico de series estacionaria
grafico_series_estacionaria <- function(datas, series, titulo_grafico, eixo_x, eixo_y, transf=1) {
  
  # Remover tendência e padronizar variância
  if (transf == 1) {
    series <- remove_tendencia(series)
    series <- padroniza_variancia(series)
  }
  
  # Estabilizar a série
  if (transf == 2) {
    series <- estabiliza_serie(series)
  }
  
  x <- datas[97:length(datas)]
  y <- series[8:length(series)]
  
  p <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_line(color = "#2596be") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
  
  fig <- ggplotly(p + theme(plot.title = element_text(size = 10)))
  
  return(fig)
}
# grafico_series_estacionaria(as.Date(dados_pais$date), dados_pais$confirmed, "titulo", "Data", "Novos Confirmados Diários", transf=1)
# grafico_series_estacionaria(as.Date(dados_pais$date), dados_pais$confirmed, "titulo", "Data", "Novos Confirmados Diários", transf=2)

#-----------------------------------------------------------------------------------------------

# Funçao que renderiza o grafico de series estacionaria para a variavel especificada
render_grafico_series_estacionaria <- function(input, escala=1, transf=0) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var

  df <- carregar_dados(est, cid, slider, variavel)

  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  if (transf == 1) {
    titulo <- titulo_series_res(variavel, est, cid)
  }
  if (transf == 2) {
    titulo <- titulo_series_esta(variavel, est, cid)
  } 

  p <- grafico_series_estacionaria(df$date, df[[variavel]] / escala, titulo, "Data", "Valor padronizado", transf=transf)

  return(p)
}