# Funçao geral para o grafico de series estacionaria

#-----------------------------------------------------------------------------------------------

# Funçao que gera o grafico de series estacionaria
grafico_series_estacionaria <- function(datas, series, titulo_grafico, eixo_x, eixo_y, saz=F) {
  tendencias <- estima_tendencia(series)
  sazonalidade <- estima_sazonalidade(diff(series - tendencias), datas[2:length(datas)])
  p <- as.data.frame(cbind(serie=series)) %>%
    slice(-1) %>%
    mutate(serie_mutada = diff(series))
  if (saz) {
    p <- p %>%
    ggplot(aes(x = datas[2:length(datas)], y = serie_mutada-diff(tendencias))) +
    geom_line(color = "#2596be") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y") +
    geom_line(aes(x=datas[2:length(datas)], y=sazonalidade), color="#be4d25", size = 0.15)
  }
  else {
    p <- p %>%
      ggplot(aes(x = datas[2:length(datas)], y = serie_mutada-diff(tendencias) - sazonalidade)) +
      geom_line(color = "#2596be") +
      labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
      theme_minimal() +
      scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
  }
  
  fig <- ggplotly(p)
  
  return(fig)
  
}

#-----------------------------------------------------------------------------------------------

# Funçao que renderiza o grafico de series estacionaria para a variavel especificada
render_grafico_series_estacionaria <- function(input, variavel, saz=F, escala=1) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider

  df <- carregar_dados(est, cid, slider, variavel)

  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  titulo <- titulo_series(variavel, est, cid)

  p <- grafico_series_estacionaria(df$date, df[[variavel]] / escala, titulo, "Data", "Novos Confirmados Diários", saz=saz)

  return(p)
}