
grafico_vac_comp_geo <- function(input,output){
  est1 <- input$e_c1
  cid1 <- input$cidade_filtro1
  est2 <- input$e_c2
  cid2 <- input$cidade_filtro2
  
  
  if((is.null(est1) && is.null(est2)) || (est1 == '' && est2 == '') || (is.null(est1) || is.null(est2)) || (est1 == '' || est2 == '')) {
    
    df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$data_slider1[1],
                                                                                date <= input$data_slider1[2])
    
    p <- df_cidade %>%
      slice(-1) %>%
      mutate(vaccines = diff(df_cidade$vaccines)) %>%
      ggplot(aes(x = date, y = vaccines)) +
      geom_line(color = "blue") +
      labs(title = paste("Doses de vacinas administradas no Brasil"), x = "Data", y = "Novos Confirmados Diários") +
      theme_minimal() +
      scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
    
    fig <- ggplotly(p)
    return(fig)
    
  } else {
    if(!((is.null(est1) && is.null(est2)) || (est1 == '' && est2 == '')) && file.exists(paste('dados/dados', est1, cid1, '.csv')) && file.exists(paste('dados/dados', est2, cid2, '.csv'))) {
      df_cidade1 <- read.csv(paste('dados/dados', est1, cid1, '.csv')) %>% filter(date >= input$data_slider1[1],
                                                                                  date <= input$data_slider1[2])
      df_cidade1$date <- as.Date(df_cidade1$date)
      df_cidade1 <- corrige(df_cidade1,"vaccines")
      df_cidade1 <- df_cidade1 %>%
        slice(-1) %>%
        mutate(vaccines = diff(df_cidade1$vaccines))
      
      df_cidade2 <- read.csv(paste('dados/dados', est2, cid2, '.csv')) %>% filter(date >= input$data_slider1[1],
                                                                                  date <= input$data_slider1[2])
      df_cidade2$date <- as.Date(df_cidade2$date)
      df_cidade2 <- corrige(df_cidade2,"vaccines")
      df_cidade2 <- df_cidade2 %>%
        slice(-1) %>%
        mutate(vaccines = diff(df_cidade2$vaccines))
      
      df_1e2 <- rbind(df_cidade1,df_cidade2)
      
      p <- ggplot(df_1e2, aes(x = date, y = vaccines, color = administrative_area_level_3)) +
        geom_line() +
        labs(title = paste("Doses de vacinas administradas em", cid1,"e", cid2),
             x = "Ano",
             y = "Confirmados")
      
      fig <- ggplotly(p)
      return(fig)
    }
    else {
      df_cidade1 <- dados_estados %>% filter(administrative_area_level_2 == est1) %>% filter(date >= input$data_slider1[1],
                                                                                             date <= input$data_slider1[2])
      df_cidade1$date <- as.Date(df_cidade1$date)
      df_cidade1 <- corrige(df_cidade1,"vaccines")
      df_cidade1 <- df_cidade1 %>%
        slice(-1) %>%
        mutate(vaccines = diff(df_cidade1$vaccines))
      
      df_cidade2 <- dados_estados %>% filter(administrative_area_level_2 == est2) %>% filter(date >= input$data_slider1[1],
                                                                                             date <= input$data_slider1[2])
      df_cidade2$date <- as.Date(df_cidade2$date)
      df_cidade2 <- corrige(df_cidade2,"vaccines")
      df_cidade2 <- df_cidade2 %>%
        slice(-1) %>%
        mutate(vaccines = diff(df_cidade2$vaccines))
      
      df_1e2 <- rbind(df_cidade1,df_cidade2)
      
      p <- ggplot(df_1e2, aes(x = date, y = vaccines, color = administrative_area_level_2)) +
        geom_line() +
        labs(title = paste("Doses de vacinas administradas em", est1,"e", est2),
             x = "Ano",
             y = "Confirmados")
      
      fig <- ggplotly(p)
      return(fig)
    }
  }
}