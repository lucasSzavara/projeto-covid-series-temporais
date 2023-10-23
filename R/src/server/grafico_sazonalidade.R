
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
  interactive_plot <- ggplotly(G, tooltip = c("all"))
  interactive_plot <- interactive_plot %>%
    layout(showlegend = T)
  fig <- plotly_build(interactive_plot)
  hover <- c()
  for(i in 1:length(fig$x$data)){
    fig$x$data[[i]]$hovertemplate <- c(hover, paste('<b>', eixo_y, '</b>: %{y:,.0f}',
                                                    '<b>Data</b>', datas[year(datas)==str_sub(fig$x$data[[i]]$text[1], start=-4)],
                                                    '<extra></extra>'
    ))
  }
  return(fig)
}

#-----------------------------------------------------------------------------------------------

# Casos confirmados
grafico_sazonalidade_casos <- function(input,output){
  est <- input$e_c
  cid <- input$cidade_filtro
  print(est)
  print(cid)
  if(is.null(est) || est == '') {
    df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                date <= input$date_slider[2])
  } else {
    if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
      df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                               date <= input$date_slider[2])
    }
    else {
      df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                           date <= input$date_slider[2])
    }
  }
  df_cidade$date <- as.Date(df_cidade$date)
  df_cidade <- corrige(df_cidade,"confirmed")
  
  df_cidade <- df_cidade %>%
    slice(-1) %>%
    mutate(confirmed = diff(df_cidade$confirmed))
  
  p <- grafico_sazonal(df_cidade$date,df_cidade$confirmed,"Casos confirmados em anos sucessivos","Data","Novos confirmados","year")
  
  return(p)
}

#-----------------------------------------------------------------------------------------------

# Mortes
grafico_sazonalidade_mortes <- function(input,output){
  est <- input$e_c
  cid <- input$cidade_filtro
  print(est)
  print(cid)
  if(is.null(est) || est == '') {
    df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                date <= input$date_slider[2])
  } else {
    if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
      df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                               date <= input$date_slider[2])
    }
    else {
      df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                           date <= input$date_slider[2])
    }
  }
  df_cidade$date <- as.Date(df_cidade$date)
  df_cidade <- corrige(df_cidade,"deaths")
  
  df_cidade <- df_cidade %>%
    slice(-1) %>%
    mutate(deaths = diff(df_cidade$deaths))
  
  p <- grafico_sazonal(df_cidade$date,df_cidade$deaths,"Número de mortos em anos sucessivos","Data","Mortes","year")
  
  return(p)
}

#-----------------------------------------------------------------------------------------------

# Vacinas
grafico_sazonalidade_vacinas <- function(input,output){
  est <- input$e_c
  cid <- input$cidade_filtro
  print(est)
  print(cid)
  if(is.null(est) || est == '') {
    df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                date <= input$date_slider[2])
  } else {
    if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
      df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                               date <= input$date_slider[2])
    }
    else {
      df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                           date <= input$date_slider[2])
    }
  }
  df_cidade$date <- as.Date(df_cidade$date)
  df_cidade <- corrige(df_cidade,"vaccines")
  
  df_cidade <- df_cidade %>%
    slice(-1) %>%
    mutate(vaccines = diff(df_cidade$vaccines))
  
  p <- grafico_sazonal(df_cidade$date,df_cidade$vaccines,"Doses de vacinas administradas em anos sucessivos","Data","Doses de vacinas","year")
  
  return(p)
}

