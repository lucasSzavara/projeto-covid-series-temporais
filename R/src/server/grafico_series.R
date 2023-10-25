# Funçao geral para o grafico de series

#-----------------------------------------------------------------------------------------------

# Funçao que gera o grafico de series
grafico_series <- function(datas, series, titulo_grafico, eixo_x, eixo_y) {
  tendencias <- estima_tendencia(series)
  p <- as.data.frame(cbind(serie=series)) %>%
    slice(-1) %>%
    mutate(serie_mutada = diff(series)) %>%
    ggplot(aes(x = datas[2:length(datas)], y = serie_mutada)) +
    geom_line(color = "blue") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y") +
    geom_line(aes(x=datas[2:length(datas)], y=diff(tendencias)), color='red')
  
  fig <- ggplotly(p)

  return(fig)
}

#-----------------------------------------------------------------------------------------------

# Funçao que renderiza o grafico de series para a variavel especificada
render_grafico_series <- function(input, variavel, escala=1) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider

  df <- carregar_dados(est, cid, slider, variavel)

  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  titulo <- titulo_series(variavel, est, cid)

  p <- grafico_series(df$date, df[[variavel]] / escala, titulo, "Data", "Novos Confirmados Diários")
  
  p <- p %>%
    layout(showlegend = F,
           xaxis = list(rangeslider = list(visible=T)))
  
  return(p)
}