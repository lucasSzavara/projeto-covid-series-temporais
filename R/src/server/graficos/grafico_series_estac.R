# Funçao geral para o grafico de series estacionaria

#-----------------------------------------------------------------------------------------------

# Funçao que gera o grafico de series estacionaria
grafico_series_estacionaria <- function(datas, series, titulo_grafico, eixo_x, eixo_y, saz=F, transf=0) {
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
    if (transf==0) {
      x <- datas[2:length(datas)]
      y <- p$serie_mutada - diff(tendencias) - sazonalidade
      #y <- p$serie_mutada - diff(tendencias)
      #ysd <- rollapply(y, width=15, FUN = sd, fill = NA)
      #index <- which(!is.na(ysd))
      #x <- x[index]
      #y <- y[index]/ysd[index]
      #p <- p[index,1:2] %>%
      p <- p[,1:2] %>%
        ggplot(aes(x = x, y = y)) +
        geom_line(color = "#2596be") +
        labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
        theme_minimal() +
        scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
    }
    if (transf==1) {
      x <- datas[2:length(datas)]
      #y <- p$serie_mutada - diff(tendencias) - sazonalidade
      y <- p$serie_mutada - diff(tendencias)
      ysd <- rollapply(y, width=180, FUN = sd, fill = NA)
      index <- which(!is.na(ysd))
      x <- x[index]
      y <- y[index]/ysd[index]
      p <- p[index,1:2] %>%
        ggplot(aes(x = x, y = y)) +
        geom_line(color = "#2596be") +
        labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
        theme_minimal() +
        scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
    }
    if (transf==2) {
      x <- datas[2:length(datas)]
      #y <- p$serie_mutada - diff(tendencias) - sazonalidade
      y <- p$serie_mutada - diff(tendencias)
      ysd <- rollapply(y, width=180, FUN = sd, fill = NA)
      index <- which(!is.na(ysd))
      x <- x[index]
      y <- y[index]/ysd[index]
      sazonalidade <- estima_sazonalidade(y, x)
      p <- p[index,1:2] %>%
        ggplot(aes(x = x, y = y - sazonalidade)) +
        geom_line(color = "#2596be") +
        labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
        theme_minimal() +
        scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
    }
    if (transf==3) {
      x <- datas[2:length(datas)]
      #y <- p$serie_mutada - diff(tendencias) - sazonalidade
      y <- p$serie_mutada - diff(tendencias)
      ysd <- rollapply(y, width=25, FUN = sd, fill = NA)
      index <- which(!is.na(ysd))
      x <- x[index]
      y <- y[index]/ysd[index]
      p <- p[index[9:length(index)],1:2] %>%
        ggplot(aes(x = x[9:length(x)], y = diff(diff(y,lag=7)))) +
        geom_line(color = "#2596be") +
        labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
        theme_minimal() +
        scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
    }
  }

      
  fig <- ggplotly(p + theme(plot.title=element_text(size=10)))
  
  return(fig)
  
}

#-----------------------------------------------------------------------------------------------

# Funçao que renderiza o grafico de series estacionaria para a variavel especificada
render_grafico_series_estacionaria <- function(input, saz=F, escala=1, transf=0) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var

  df <- carregar_dados(est, cid, slider, variavel)

  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  if (saz) {
    titulo <- titulo_series_res(variavel, est, cid) 
  }
  
  else {
    titulo <- titulo_series_esta(variavel, est, cid) 
  }

  p <- grafico_series_estacionaria(df$date, df[[variavel]] / escala, titulo, "Data", "Novos Confirmados Diários", saz=saz, transf=transf)

  return(p)
}