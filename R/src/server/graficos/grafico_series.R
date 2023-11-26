# Funçao geral para o grafico de series

#-----------------------------------------------------------------------------------------------

# Funçao que gera o grafico de series
grafico_series <- function(datas, series, titulo_grafico, eixo_x, eixo_y) {
  tendencias <- estima_tendencia(series)
  p <- data.frame(datas=datas[2:length(datas)],series=diff(series),tendencias=diff(tendencias))
  
  fig <- ggplot(p,aes(x=datas))+
    geom_line(aes(y = series, color = "Série"), linewidth = 1) +
    geom_line(aes(y = tendencias, color = "Tendência"), linewidth = 1) +
    scale_color_manual(values = c("Série" = "#2596be", "Tendência" = "#be4d25")) +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
  
  fig <- ggplotly(fig)
  
  return(fig)
}

#-----------------------------------------------------------------------------------------------

# Funçao que renderiza o grafico de series para a variavel especificada
render_grafico_series <- function(input) {
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
  
  p <- grafico_series(df$date, df[[variavel]] / escala, titulo, "Data", "Novos Confirmados Diários")
  
  return(p)
}