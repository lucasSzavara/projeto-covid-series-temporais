# Função geral para o gráfico de autocorrelação

#-----------------------------------------------------------------------------------------------

# Função que gera o gráfico de autocorrelação
grafico_ACF <- function(df, variavel, escala, titulo_grafico, eixo_x, eixo_y) {
  # Ajustar escala
  df <- df %>%
    mutate(!!variavel := df[[variavel]] / escala)
  
  # Estimar a tendência
  tendencias <- estima_tendencia(df[[variavel]])
  
  # Calcular a série sem a tendência
  df_sem_tendencia <- df %>%
    slice(-1) %>%
    mutate(sem_tendencia = diff(df[[variavel]]) - diff(tendencias))
  
  sazonalidade <- estima_sazonalidade(df_sem_tendencia$sem_tendencia, df_sem_tendencia$date)
  
  # Autocorrelação depois de retirar a tendência
  dados = tsibble(
    data = df_sem_tendencia$date,
    y = df_sem_tendencia$sem_tendencia - sazonalidade,
    index = data
  )
  
  # Gerar o gráfico
  G <- 
    dados %>% 
    ACF() %>% 
    autoplot() +
    labs(
      x = eixo_x,
      y = eixo_y,
      title = titulo_grafico
    ) +
    coord_cartesian(ylim=c(-1,1)) +
    theme_minimal()
  
  fig <- ggplotly(G + theme(plot.title=element_text(size=10))
)
  
  return(fig)
}

# grafico_ACF(dados_pais, "confirmed", 10000) # testar a funçao

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

