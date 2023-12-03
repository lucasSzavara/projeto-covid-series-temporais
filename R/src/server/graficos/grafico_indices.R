# Funçao geral para o grafico de series para analisar os índices políticos

#-----------------------------------------------------------------------------------------------

# Funçao que gera o grafico de series
grafico_series_ind <- function(datas, series, indice, titulo_grafico, eixo_x, eixo_y, legenda) {
  
  p <- as.data.frame(cbind(serie=series, indice_politico=indice)) %>%
    slice(-1) %>%
    mutate(serie_mutada = diff(series)) %>%
    ggplot(aes(x = datas[2:length(datas)], y = serie_mutada, colour = indice_politico)) +
    geom_point() +
    scale_colour_gradientn(colours=rainbow(4)) +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y, color = legenda) +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
  
  fig <- ggplotly(p)
  
  return(fig)
}

#-----------------------------------------------------------------------------------------------

# Funçao que renderiza o grafico de series para a variavel especificada
render_grafico_series_ind <- function(input) {
  est <- input$e_c_ind
  cid <- input$cidade_filtro_ind
  slider <- input$date_slider_ind
  indice <- input$ind_pol
  variavel <- input$var3
  
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
  
  titulo <- titulo_series(variavel, est, cid)
  
  p <- grafico_series_ind(df$date, df[[variavel]] / escala, df[[indice]], titulo, "Data", "Novos Confirmados Diários",indice)
  
  return(p)
}