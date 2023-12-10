# Função geral para o gráfico de autocorrelação

#-----------------------------------------------------------------------------------------------

# Função que gera o gráfico de autocorrelação
grafico_ACF <- function(df, variavel, escala, titulo_grafico, eixo_x, eixo_y) {
  
  # Ajustar escala
  df <- df %>%
    mutate(!!variavel := df[[variavel]] / escala)
  
  # Estabilizar a tendência
  dados <- estabiliza_serie(df[[variavel]])
  
  # Criar um objeto tsibble com as datas ajustadas
  dados = tsibble(
    data = as.Date(df$date[180:nrow(df)]),
    y = dados,
    index = data
  )

  # Calcular q
  soma <- calcula_q(dados)
  
  # Montar grafico
  G <- 
    dados %>% 
    ACF(lag_max=90) %>% 
    autoplot() +
    labs(
      x = eixo_x,
      y = eixo_y,
      title = titulo_grafico
    ) +
    coord_cartesian(ylim=c(-1,1)) +
    theme_minimal() +
    geom_vline(xintercept = soma, linetype = "dotted", color = "red") 
  
  
  # Tornar o gráfico interativo
  fig <- ggplotly(G + theme(plot.title=element_text(size=10))
  )
  
  return(fig)
}
# grafico_ACF(dados_pais, "confirmed", 10000, "", "", "") # testar a funçao

#-----------------------------------------------------------------------------------------------

# Função que renderiza o gráfico de autocorrelação para a variável especificada
render_grafico_ACF <- function(input, escala = 1, eixo_x = "Defasagem", eixo_y = "Autocorrelação") {
  # Extrair informações do input
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var
  
  # Carregar dados
  df <- carregar_dados(est, cid, slider, variavel, TRUE)
  
  # Verificar se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  # Calcular o título do gráfico
  titulo <- titulo_series_acf(variavel, est, cid)
  
  # Gerar o gráfico
  p <- grafico_ACF(df, variavel, escala, titulo, eixo_x, eixo_y)
  
  return(p)
}