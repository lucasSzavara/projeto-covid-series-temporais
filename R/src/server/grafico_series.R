
# Casos confirmados
grafico_series_casos <- function(input,output){
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
  
  p <- df_cidade %>%
    slice(-1) %>%
    mutate(confirmed = diff(df_cidade$confirmed)) %>%
    ggplot(aes(x = date, y = confirmed)) +
    geom_line(color = "blue") +
    labs(title = paste("Casos confirmados em", cid, ', ', est), x = "Data", y = "Novos Confirmados Diários") +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
  
  fig <- ggplotly(p)
  return(fig)
}

#-----------------------------------------------------------------------------------------------

# Mortes

grafico_series_mortes <- function(input,output){
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
  
  p <- df_cidade %>%
    slice(-1) %>%
    mutate(deaths = diff(df_cidade$deaths)) %>%
    ggplot(aes(x = date, y = deaths)) +
    geom_line(color = "red") +
    labs(title = paste("Número de mortos em", cid, ', ', est), x = "Data", y = "Número de Mortes Diárias") +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
  
  fig <- ggplotly(p)
  return(fig)
}

#-----------------------------------------------------------------------------------------------

# Vacinas

grafico_series_vacinas <- function(input,output){
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
  
  p <- df_cidade %>%
    slice(-1) %>%
    mutate(vaccines = diff(df_cidade$vaccines)) %>%
    ggplot(aes(x = date, y = vaccines)) +
    geom_line(color = "green") +
    labs(title = paste("Doses de vacinas administradas em", cid, ', ', est), x = "Data", y = "Doses de vacinas Diárias") +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
  
  fig <- ggplotly(p)
  return(fig)
}

