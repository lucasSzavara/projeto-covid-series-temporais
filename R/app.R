# Pacotes
#install.packages("pacman")
pacman::p_load(shiny, shinydashboard, shinythemes, COVID19, fpp3, plotly, forecast, stringr)


#------------------------------------------------------------

# Dados para teste

dados_estados <- covid19(country = c('Brazil'), level=2, verbose=F)
# dados$date <- as.Date(dados$date, format = "%Y-%m-%d")
locais <- read.csv('estados_cidades.csv')
# Filtragem dos dados
# df_confirmed <- dados[,c("id","date","confirmed","administrative_area_level_2","administrative_area_level_3")]
# df_confirmed <- na.omit(df_confirmed)
#------------------------------------------------------------

# Selecionadores

est <- c('', unique(locais$estados))

inicio <- c("Analisar apartir de X confirmados")

#------------------------------------------------------------

# Funções

corrige <- function(df_, variavel){
  df <- df_
  df[is.na(df)] <- 0
  for(i in 2:length(df[[variavel]])){
    if(df[i,variavel] < df[i-1,variavel]){
      df[i,variavel] <- df[i-1,variavel]
    }
  }
  return(df)
}

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

#------------------------------------------------------------

# Defina o UI
ui <- dashboardPage(
  dashboardHeader(title = "Covid-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "vg", icon = icon("virus")),
      menuItem("Evolução", tabName = "vis", icon = icon("th")),
      menuItem("Diferenças geográficas", tabName = "dg", icon = icon("chart-line")),
      menuItem("Efeito de medidas políticas", tabName = "efeito",icon = icon("globe")),
      menuItem("Índices", tabName = "ind",icon = icon("signal"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "vg",
              h2("Visão Geral"),
              p("Menu sobre covid-19 no Brasil")
              # Adicione elementos específicos para a Página 1 aqui
      ),
      tabItem(tabName = "vis",
              fluidRow(
                h2(" 1) Confirmados"),
                column(width = 2, 
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("e_c", "Estado", est, selectize = TRUE)),
                       uiOutput('html_filtro_cidade'),
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           sliderInput("date_slider", "Período", min = min(dados_estados$date), max = max(dados_estados$date),
                                       value = c(min(dados_estados$date), max(dados_estados$date)))),
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("inicio", "Data de início do gráfico", inicio))),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series", height = 500)))
              ),
              
              fluidRow(
                column(width = 2
                ),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_saz", height = 500)))
              ),
              
              fluidRow(
                h2(" 2) Mortalidade"),
                column(width = 2
                ),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series1", height = 500)))
              ),
              
              fluidRow(
                column(width = 2
                ),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_saz1", height = 500)))
              ),
              
              fluidRow(
                h2(" 3) Doses de Vacinas administradas"),
                column(width = 2
                ),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series2", height = 500)))
              ),
              
              fluidRow(
                column(width = 2
                ),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_saz2", height = 500)))
              ),
      ),
      tabItem("dg",
              fluidRow(
                h2("1) Doses de Vacinas administradas em duas áreas administrativas"),
                column(width = 2, 
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("e_c1", "Estado1", est, selectize = TRUE),
                           uiOutput('html_filtro_cidade1')),
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("e_c2", "Estado2", est, selectize = TRUE),
                           uiOutput('html_filtro_cidade2')),
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           sliderInput("data_slider1", "Período", min = min(dados_estados$date), max = max(dados_estados$date),
                                       value = c(min(dados_estados$date), max(dados_estados$date)))),
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("inicio1", "Data de início do gráfico", inicio))),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series_1e2", height = 500)))
              ),
              # Adicione elementos específicos para a Página 3 aqui
      ),
      tabItem("efeito",
              h2("Conteúdo da Página 4"),
              # Adicione elementos específicos para a Página 4 aqui
      ),
      tabItem("ind",
              h2("Conteúdo da Página 5"),
              # Adicione elementos específicos para a Página 5 aqui
      )
    )
  )
)

#------------------------------------------------------------

# Defina o servidor
server <- function(input, output, session) {
  
  # Lógica ou ações específicas para cada página podem ser adicionadas aqui
  
  print('recarregando')
  #================================================================== EVOLUÇÃO
  
  output$html_filtro_cidade <- renderUI({
    est <- input$e_c
    if(!(is.null(est) || est == '')) {
      cidades <- unique(locais[locais$estados == est, ]$cidades)
      return(box(width = NULL, status = "warning", solidHeader = TRUE,
                 selectInput("cidade_filtro", "Cidade", c('', cidades), selectize = TRUE)))
    }
  })
  observe({
    est <- input$e_c
    updateSelectInput(session, 'cidade_filtro', selected='')
  })
  
  
  output$grafico_series <- renderPlotly({
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
  })
  
  output$grafico_saz <- renderPlotly({
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
  })
  
  
  output$grafico_series1 <- renderPlotly({
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
  })
  
  
  output$grafico_saz1 <- renderPlotly({
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
  })
  
  
  output$grafico_series2 <- renderPlotly({
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
  })
  
  output$grafico_saz2 <- renderPlotly({
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
  })
  
  #================================================================== END: EVOLUÇÃO
  
  #================================================================== DIFERENÇAS GEOGRÁFICAS
  
  output$html_filtro_cidade1 <- renderUI({
    est <- input$e_c1
    if(!(is.null(est) || est == '')) {
      cidades <- unique(locais[locais$estados == est, ]$cidades)
      return(box(width = NULL, status = "warning", solidHeader = TRUE,
                 selectInput("cidade_filtro1", "Cidade 1", c('', cidades), selectize = TRUE)))
    }
  })
  observe({
    est <- input$e_c1
    updateSelectInput(session, 'cidade_filtro1', selected='')
  })
  
  output$html_filtro_cidade2 <- renderUI({
    est <- input$e_c2
    if(!(is.null(est) || est == '')) {
      cidades <- unique(locais[locais$estados == est, ]$cidades)
      return(box(width = NULL, status = "warning", solidHeader = TRUE,
                 selectInput("cidade_filtro2", "Cidade 2", c('', cidades), selectize = TRUE)))
    }
  })
  observe({
    est <- input$e_c2
    updateSelectInput(session, 'cidade_filtro2', selected='')
  })
  
  
  output$grafico_series_1e2 <- renderPlotly({
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
  })
  
  #================================================================== END: DIFERENÇAS GEOGRÁFICAS
  
}

#------------------------------------------------------------

# Crie o aplicativo Shiny
shinyApp(ui, server)