# Pacotes
#install.packages("pacman")
pacman::p_load(shiny, shinydashboard, shinythemes, COVID19, fpp3, plotly, forecast)


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


#------------------------------------------------------------

# Defina o UI
ui <- dashboardPage(
  dashboardHeader(title = "Covid-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "vg", icon = icon("virus")),
      menuItem("Evolução", tabName = "vis", icon = icon("th")),
      menuItem("Diferenças geográficas", tabName = "ev", icon = icon("chart-line")),
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
                h2(" 2) Mortalidade"),
                column(width = 2
                ),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series1", height = 500)))
              ),
              
              fluidRow(
                h2(" 3) Doses de Vacinas administradas"),
                column(width = 2
                ),
                column(width = 10, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series2", height = 500)))
              )
      ),
      tabItem("ev",
              h2("Conteúdo da Página 3"),
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
      labs(title = paste("Casos de mortalidade em", cid, ', ', est), x = "Data", y = "Número de Mortes Diárias") +
      theme_minimal() +
      scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
    
    fig <- ggplotly(p)
    return(fig)
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
  
  
}

#------------------------------------------------------------

# Crie o aplicativo Shiny
shinyApp(ui, server)