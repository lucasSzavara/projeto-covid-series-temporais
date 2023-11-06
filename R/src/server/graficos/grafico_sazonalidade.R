# Remover
dados_estados <- read.csv('dados/pais/dados_pais.csv')
# Remover

# Gráfico sazonalidade

grafico_sazonal <- function(datas, serie, titulo_grafico, eixo_x, eixo_y, periodo) {
  dados = tsibble(
    data = datas,
    y = serie,
    index = data
  )
  
  dados <- dados %>%
    fill_gaps(data, .full=FALSE)
  
  # Plotando a série temporal
  G =
    dados %>%
    gg_season(y, period=periodo) +
    labs(
      y = eixo_y,
      x = eixo_x,
      title = titulo_grafico,
      labels = "right"
    ) +
    guides(color = guide_legend(title = "Legenda"))
  interactive_plot <- ggplotly(G + theme(plot.title=element_text(size=10)), tooltip = c("all"))
  interactive_plot <- interactive_plot %>%
    layout(showlegend = T)
  fig <- plotly_build(interactive_plot)
  hover <- c()
  
  if (periodo == "year") {
    for(i in 1:length(fig$x$data)){
      fig$x$data[[i]]$hovertemplate <- c(hover, paste('<b>', eixo_y, '</b>: %{y:,.0f}',
                                                      '<b>Data</b>', datas[year(datas)==str_sub(fig$x$data[[i]]$text[1], start=-4)],
                                                      '<extra></extra>'
      ))
    }
  }
  
  if (periodo == "week") {
    week <- isoweek(datas)
    for (i in 1:length(week)) {
      if (nchar(week[i]) == 1) {
        week[i] <- paste0(0,week[i])
      }
    }
    
    if (length(datas[(week==str_sub(fig$x$data[[1]]$text[1], start=-2))
                     & (year(datas)==str_sub(fig$x$data[[1]]$text[1], start=-8, end=-5))]) != 0) {
      for(i in 1:length(fig$x$data)) {
        fig$x$data[[i]]$hovertemplate <- c(hover, paste('<b>', eixo_y, '</b>: %{y:,.0f}',
                                                        '<b>Data</b>', datas[(week==str_sub(fig$x$data[[i]]$text[1], start=-2))
                                                                             & (year(datas)==str_sub(fig$x$data[[i]]$text[1], start=-8, end=-5))],
                                                        '<extra></extra>'
        ))
      }
    } else {
      for(i in 1:length(fig$x$data)){
        fig$x$data[[i]]$hovertemplate <- c(hover, paste('<b>', eixo_y, '</b>: %{y:,.0f}',
                                                        '<b>Data</b>', datas[(week==str_sub(fig$x$data[[i]]$text[1], start=-2))],
                                                        '<extra></extra>'
        ))
      }  
    }
  }
  
  return(fig)
}

#-----------------------------------------------------------------------------------------------

render_grafico_sazonalidade <- function(input, escala=1){
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider
  variavel <- input$var
  periodo <- input$periodo
  
  df <- carregar_dados(est, cid, slider, variavel)
  
  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  titulo <- titulo_saz(variavel, est, cid, periodo)
  
  if (variavel == "confirmed") {
    eixo_y <- paste("Casos confirmados")
    df <- df %>%
      slice(-1) %>%
      mutate(confirmed = diff(df[[variavel]]))
  } else {
    if (variavel == "deaths") {
      eixo_y <- paste("Número de mortos")
      df <- df %>%
        slice(-1) %>%
        mutate(deaths = diff(df[[variavel]]))
    } else {
      eixo_y <- paste("Doses administradas/10000")
      df <- df %>%
        slice(-1) %>%
        mutate(vaccines = diff(df[[variavel]]))
    }
  }
  
  p <- grafico_sazonal(df$date,df[[variavel]] / escala, titulo,"Data",eixo_y,periodo)
  
  return(p)
  
}

