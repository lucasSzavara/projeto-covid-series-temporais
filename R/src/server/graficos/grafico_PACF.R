# Função geral para o gráfico de autocorrelação parcial

#-----------------------------------------------------------------------------------------------

# Função que gera o gráfico de autocorrelação parcial
grafico_PACF <- function(df, variavel, escala, titulo_grafico, eixo_x, eixo_y, transf=0) {
  # Ajustar escala
  df <- df %>%
    mutate(!!variavel := df[[variavel]] / escala)
  
  # Estimar a tendência
  tendencias <- estima_tendencia(df[[variavel]])
  
  # Calcular a série sem a tendência
  df_sem_tendencia <- df %>%
    slice(-1) %>%
    mutate(sem_tendencia = diff(df[[variavel]]) - diff(tendencias))
  
  if (transf==0) {
    dados = tsibble(
      data = df_sem_tendencia$date,
      y = df_sem_tendencia$sem_tendencia - sazonalidade,
      index = data
    )
    
    G <- 
      dados %>% 
      PACF() %>%
      autoplot() +
      labs(
        x = eixo_x,
        y = eixo_y,
        title = titulo_grafico
      ) +
      coord_cartesian(ylim=c(-1,1)) +
      theme_minimal()
  }
  
  if (transf==1) {
    #y <- df_sem_tendencia$sem_tendencia - sazonalidade
    y <- df_sem_tendencia$sem_tendencia
    ysd <- rollapply(y, width=180, FUN = sd, fill = NA)
    index <- which(!is.na(ysd))
    
    dados = tsibble(
      data = df_sem_tendencia$date[index],
      y = y[index]/ysd[index],
      index = data
    )
    
    G <- 
      dados %>% 
      PACF() %>% 
      autoplot() +
      labs(
        x = eixo_x,
        y = eixo_y,
        title = titulo_grafico
      ) +
      coord_cartesian(ylim=c(-1,1)) +
      theme_minimal()
  }
  
  if (transf==2) {
    #y <- df_sem_tendencia$sem_tendencia - sazonalidade
    y <- df_sem_tendencia$sem_tendencia
    ysd <- rollapply(y, width=180, FUN = sd, fill = NA)
    index <- which(!is.na(ysd))
    y <- y[index]/ysd[index]
    
    dados = tsibble(
      data = df_sem_tendencia$date[index][8:length(index)],
      y = diff(y, lag=7),
      index = data
    )
    
    pacf_values <- PACF(dados, lag_max=90)$pacf
    intercept_ind <- which(abs(pacf_values) < qnorm((1 + 0.96) / 2) / sqrt(length(dados$y)))
    comp <- which(c(diff(intercept_ind),2) !=1)
    inds <- c(comp[1],abs(rev(diff(rev(comp)))))
    ind <- which(c(comp[1],abs(rev(diff(rev(comp))))) > 5)[1]
    soma <- sum(inds[1:ind-1])
    valor <- comp[ind]
    complemento <- valor - sum(1:valor %in% intercept_ind)
    soma <- soma + complemento
    
    G <- 
      dados %>% 
      PACF(lag_max=90) %>% 
      autoplot() +
      labs(
        x = eixo_x,
        y = eixo_y,
        title = titulo_grafico
      ) +
      coord_cartesian(ylim=c(-1,1)) +
      theme_minimal() +
      geom_vline(xintercept = soma, linetype = "dotted", color = "red")
  }
  
  if (transf==3) {
    #y <- df_sem_tendencia$sem_tendencia - sazonalidade
    y <- df_sem_tendencia$sem_tendencia
    ysd <- rollapply(y, width=25, FUN = sd, fill = NA)
    index <- which(!is.na(ysd))
    
    dados = tsibble(
      data = df_sem_tendencia$date[index[9:length(index)]],
      y = diff(diff(y[index]/ysd[index],lag=7)),
      index = data
    )
    
    G <- 
      dados %>% 
      PACF() %>% 
      autoplot() +
      labs(
        x = eixo_x,
        y = eixo_y,
        title = titulo_grafico
      ) +
      coord_cartesian(ylim=c(-1,1)) +
      theme_minimal()
  }
  
  fig <- ggplotly(G + theme(plot.title=element_text(size=10)))
  
  return(fig)
}

# Função que renderiza o gráfico de autocorrelação parcial para a variável especificada
render_grafico_PACF <- function(input, escala = 1, eixo_x = "Defasagem", eixo_y = "Autocorrelação Parcial", transf=0) {
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
  titulo <- titulo_series_pacf(variavel, est, cid)
  
  # Gerar o gráfico
  p <- grafico_PACF(df, variavel, escala, titulo, eixo_x, eixo_y, transf)
  
  return(p)
}
