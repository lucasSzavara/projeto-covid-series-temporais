# Pacotes
# install.packages("pacman")
pacman::p_load(shiny, 
               shinydashboard, 
               shinythemes, 
               COVID19, 
               fpp3, 
               plotly, 
               forecast, 
               stringr, 
               drc)



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


est <- c('', sort(unique(locais$estados)))

inicio <- c("Analisar apartir de X confirmados")

#------------------------------------------------------------

# Funções

source("./src/funcoes/corrige_serie_acumulada.R")
source("./src/server/grafico_series.R")
source("./src/server/grafico_sazonalidade.R")
source("./src/server/comp_geo_vacinas.R")
source("./src/server/tendencia.R")

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
                column(width = 4,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("e_c", "Estado", est, selectize = TRUE)
                       )
                ),
                
                column(width = 4,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           uiOutput('html_filtro_cidade')
                       )
                ),
                
                column(width = 4,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           sliderInput("date_slider", "Período", min = min(dados_estados$date), max = max(dados_estados$date),
                                       
                                       value = c(min(dados_estados$date), max(dados_estados$date)))
                       )
                )
                
              ),
              
              fluidRow(
                column(width = 12, 
                       h2(" 1) Confirmados")
                )
              ),
              
              fluidRow(
                column(width = 6, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series", height = 500)
                       )
                ),
                
                column(width = 6, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_saz", height = 500)
                       )
                )
              ),
              
              
              
              fluidRow(
                column(width = 12, 
                       h2(" 2) Mortalidade")
                )
              ),
              
              fluidRow(
                column(width = 6, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series1", height = 500)
                       )
                ),
                
                column(width = 6, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_saz1", height = 500)
                       )
                )
              ),
              
              fluidRow(
                column(width = 12, 
                       h2(" 3) Doses de Vacinas administradas")
                )
              ),
              
              fluidRow(
                column(width = 6, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_series2", height = 500)
                       )
                ),
                column(width = 6, 
                       box(width = NULL, solidHeader = TRUE, 
                           plotlyOutput("grafico_saz2", height = 500)
                       )
                )
              )
      ),
      
      tabItem("dg",
              fluidRow(
                column(width = 4, 
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("e_c1", "Estado1", est, selectize = TRUE),
                           uiOutput('html_filtro_cidade1')
                       ),
                ),
                
                column(width = 4,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("e_c2", "Estado2", est, selectize = TRUE),
                           uiOutput('html_filtro_cidade2'))
                ),
                
                column(width = 4,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           sliderInput("data_slider1", "Período", min = min(dados_estados$date), max = max(dados_estados$date),
                                       
                                       value = c(min(dados_estados$date), max(dados_estados$date)))
                       )
                       
                ),
                
                fluidRow(
                  column(width = 12, 
                         h2(" 1) Doses de Vacinas administradas em duas áreas administrativas")
                  )
                ),
                
                fluidRow(column(width = 12, 
                                box(width = NULL, solidHeader = TRUE, 
                                    plotlyOutput("grafico_series_1e2", height = 500))
                )
                )
              )
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
      cidades <- sort(unique(locais[locais$estados == est, ]$cidades))
      return(selectInput("cidade_filtro", "Cidade", c('', cidades), selectize = TRUE))
    }
  })
  observe({
    est <- input$e_c
    updateSelectInput(session, 'cidade_filtro', selected='')
  })
  
  
  output$grafico_series <- renderPlotly({
    grafico_series_casos(input,output)
  })
  
  output$grafico_saz <- renderPlotly({
    grafico_sazonalidade_casos(input,output)
  })
  
  
  output$grafico_series1 <- renderPlotly({
    grafico_series_mortes(input,output)
  })
  
  
  output$grafico_saz1 <- renderPlotly({
    grafico_sazonalidade_mortes(input,output)
  })
  
  
  output$grafico_series2 <- renderPlotly({
    grafico_series_vacinas(input,output)
  })
  
  output$grafico_saz2 <- renderPlotly({
    grafico_sazonalidade_vacinas(input,output)
  })
  
  #================================================================== END: EVOLUÇÃO
  
  #================================================================== DIFERENÇAS GEOGRÁFICAS
  
  output$html_filtro_cidade1 <- renderUI({
    est <- input$e_c1
    if(!(is.null(est) || est == '')) {
      cidades <- sort(unique(locais[locais$estados == est, ]$cidades))
      return(selectInput("cidade_filtro1", "Cidade 1", c('', cidades), selectize = TRUE))
    }
  })
  observe({
    est <- input$e_c1
    updateSelectInput(session, 'cidade_filtro1', selected='')
  })
  
  output$html_filtro_cidade2 <- renderUI({
    est <- input$e_c2
    if(!(is.null(est) || est == '')) {
      cidades <- sort(unique(locais[locais$estados == est, ]$cidades))
      return(selectInput("cidade_filtro2", "Cidade 2", c('', cidades), selectize = TRUE))
    }
  })
  observe({
    est <- input$e_c2
    updateSelectInput(session, 'cidade_filtro2', selected='')
  })
  
  
  output$grafico_series_1e2 <- renderPlotly({
    grafico_vac_comp_geo(input,output)
  })
  
  #================================================================== END: DIFERENÇAS GEOGRÁFICAS
  
}

#------------------------------------------------------------

# Crie o aplicativo Shiny
shinyApp(ui, server)