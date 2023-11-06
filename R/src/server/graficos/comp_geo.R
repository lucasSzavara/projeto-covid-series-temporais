grafico_comp_geo_default <- function(datas, series, variavel){
  if (variavel == "confirmed") {
    p <- as.data.frame(cbind(serie=series)) %>%
      slice(-1) %>%
      mutate(serie_mutada = diff(series)) %>%
      ggplot(aes(x = datas[2:length(datas)], y = serie_mutada)) +
      geom_line(color = "#2596be") +
      labs(title = "Casos confirmados no Brasil", x = "Ano", y = "Confirmados") +
      theme_minimal() +
      scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
    
    fig <- ggplotly(p)
    
    return(fig)
  } else {
    if (variavel == "deaths") {
      p <- as.data.frame(cbind(serie=series)) %>%
        slice(-1) %>%
        mutate(serie_mutada = diff(series)) %>%
        ggplot(aes(x = datas[2:length(datas)], y = serie_mutada)) +
        geom_line(color = "#2596be") +
        labs(title = "Mortalidade no Brasil", x = "Ano", y = "Número de mortos") +
        theme_minimal() +
        scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
      
      fig <- ggplotly(p)
      
      return(fig)
    } else {
      p <- as.data.frame(cbind(serie=series)) %>%
        slice(-1) %>%
        mutate(serie_mutada = diff(series)) %>%
        ggplot(aes(x = datas[2:length(datas)], y = serie_mutada)) +
        geom_line(color = "#2596be") +
        labs(title = "Doses de vacinas administradas/10000 no Brasil", x = "Ano", y = "Doses administradas/10000") +
        theme_minimal() +
        scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
      
      fig <- ggplotly(p)
      
      return(fig)
    }
  }
}

grafico_comp_geo_estados <- function(df12,variavel,escala,est1,est2){
  if (variavel == "confirmed") {
    p <- ggplot(df12, aes(x = date, y = confirmed/escala, color = administrative_area_level_2)) +
      geom_line() +
      scale_color_manual(values = c("#2596be", "#be4d25")) +
      labs(title = paste("Casos confirmados em",est1,"e",est2),
           x = "Ano",
           y = "Confirmados")
    
    fig <- ggplotly(p)
    return(fig)
    
  } else {
    if (variavel == "deaths") {
      
      p <- ggplot(df12, aes(x = date, y = deaths/escala, color = administrative_area_level_2)) +
        geom_line() +
        scale_color_manual(values = c("#2596be", "#be4d25")) +
        labs(title = paste("Mortalidade em",est1,"e",est2),
             x = "Ano",
             y = "Número de mortos")
      
      fig <- ggplotly(p)
      return(fig)
      
    } else {
      
      p <- ggplot(df12, aes(x = date, y = vaccines/escala, color = administrative_area_level_2)) +
        geom_line() +
        scale_color_manual(values = c("#2596be", "#be4d25")) +
        labs(title = paste("Doses de vacinas administradas/10000 em",est1,"e",est2),
             x = "Ano",
             y = "Doses administradas/10000")
      
      fig <- ggplotly(p)
      return(fig)
    }
  }
}

grafico_comp_geo_cidades <- function(df12,variavel,escala,cid1,est1,cid2,est2){
  if (variavel == "confirmed") {
    p <- ggplot(df12, aes(x = date, y = confirmed/escala, color = administrative_area_level_3)) +
      geom_line() +
      scale_color_manual(values = c("#2596be", "#be4d25")) +
      labs(title = paste("Casos confirmados em",cid1,'-',est1,"e",cid2,'-',est2),
           x = "Ano",
           y = "Confirmados")
    
    fig <- ggplotly(p)
    return(fig)
    
  } else {
    if (variavel == "deaths") {
      
      p <- ggplot(df12, aes(x = date, y = deaths/escala, color = administrative_area_level_3)) +
        geom_line() +
        scale_color_manual(values = c("#2596be", "#be4d25")) +
        labs(title = paste("Mortalidade em",cid1,'-',est1,"e",cid2,'-',est2),
             x = "Ano",
             y = "Número de mortos")
      
      fig <- ggplotly(p)
      return(fig)
      
    } else {
      
      p <- ggplot(df12, aes(x = date, y = vaccines/escala, color = administrative_area_level_3)) +
        geom_line() +
        scale_color_manual(values = c("#2596be", "#be4d25")) +
        labs(title = paste("Doses de vacinas administradas/10000 em",cid1,'-',est1,"e",cid2,'-',est2),
             x = "Ano",
             y = "Doses administradas/10000")
      
      fig <- ggplotly(p)
      return(fig)
    }
  }
}

render_grafico_series_comp_geo <- function(input){
  est1 <- input$e_c1
  cid1 <- input$cidade_filtro1
  est2 <- input$e_c2
  cid2 <- input$cidade_filtro2
  slider <- input$date_slider1
  variavel <- input$var1
  
  df1 <- carregar_dados(est1, cid1, slider, variavel)
  df2 <- carregar_dados(est2, cid2, slider, variavel)
  
  if (!(variavel %in% names(df1)) || !(variavel %in% names(df2))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  if (variavel == "confirmed") {
    eixo_y <- paste("Casos confirmados")
    escala <- 1
    
    df1 <- df1 %>%
      slice(-1) %>%
      mutate(confirmed = diff(df1[[variavel]]))
    
    df2 <- df2 %>%
      slice(-1) %>%
      mutate(confirmed = diff(df2[[variavel]]))
  } else {
    if (variavel == "deaths") {
      eixo_y <- paste("Número de mortos")
      escala <- 1
      
      df1 <- df1 %>%
        slice(-1) %>%
        mutate(deaths = diff(df1[[variavel]]))
      
      df2 <- df2 %>%
        slice(-1) %>%
        mutate(deaths = diff(df2[[variavel]]))
    } else {
      eixo_y <- paste("Doses administradas/10000")
      escala <- 10000
      
      df1 <- df1 %>%
        slice(-1) %>%
        mutate(vaccines = diff(df1[[variavel]]))
      
      df2 <- df2 %>%
        slice(-1) %>%
        mutate(vaccines = diff(df2[[variavel]]))
    }
  }
  
  df_1e2 <- rbind(df1,df2)
  
  if((est1 == '' && est2 == '') || (est1 == '' || est2 == '') || (cid1 != '' && cid2 == '') || (cid1 == '' && cid2 != '')){
    dados_estados$date <- as.Date(dados_estados$date)
    df <- corrige(dados_estados, variavel)
    
    p <- grafico_comp_geo_default(df$date,df[[variavel]] / escala, variavel)
    
    return(p)
  } else if((est1 != '' && est2 != '') && (cid1 == '' && cid2 == '')){
    p <- grafico_comp_geo_estados(df_1e2,variavel,escala,est1,est2)
    
    return(p)
  } else {
    p <- grafico_comp_geo_cidades(df_1e2,variavel,escala,cid1,est1,cid2,est2)
    
    return(p)
  } 
  
}