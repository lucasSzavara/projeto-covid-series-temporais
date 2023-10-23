# Funçao geral para o grafico de series

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

render_grafico_series <- function(input, variavel, escala=1) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  
  df_cidade <- carregar_dados(est, cid, slider, variavel)
  
  if (variavel == "confirmed") {
    aux <- paste("Casos confirmados")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Número de mortos")
    } else {
      aux <- paste("Doses administradas/10000")
    }
  }
  
  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid)
    } else {
      titulo <- paste(aux, "em", est)
    }
  }
  
  p <- grafico_series(df_cidade$date, df_cidade$confirmed / escala, titulo, "Data", "Novos Confirmados Diários")
  
  return(p)
}