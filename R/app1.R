# Carregar Pacotes
# install.packages("pacman")
pacman::p_load(shiny, 
               shinydashboard, 
               shinythemes, 
               COVID19, 
               fpp3, 
               plotly, 
               forecast, 
               stringr, 
               drc,
               ggpubr)

#------------------------------------------------------------

# Carregar lista de nomes de estados e respectivas cidades
locais <- read.csv('estados_cidades.csv')

#------------------------------------------------------------

# Definir Selecionadores

est <- c('', sort(unique(locais$estados)))

#------------------------------------------------------------

# Funções

source("./src/funcoes/carregar_dados.R")
source("./src/funcoes/titulos.R")
source("./src/server/grafico_series.R")
source("./src/server/grafico_sazonalidade.R")
source("./src/server/grafico_series_estac.R")
source("./src/server/grafico_ACF.R")
source("./src/server/comp_geo_vacinas.R")
source("./src/server/tendencia.R")

#------------------------------------------------------------

# Definir o UI
ui <- dashboardPage(
# ui <- dashboardPage(skin = "purple",
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
              
              tabsetPanel(
                id = "tabs",
                tabPanel("Confirmados", 
                         conditionalPanel(
                           condition = "input.tabs == 'Confirmados'",
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
                             column(width = 6,
                                    box(width = NULL, solidHeader = TRUE,
                                        plotlyOutput("grafico_series_estac", height = 500)
                                    )
                             ),
                             
                             column(width = 6,
                                    box(width = NULL, solidHeader = TRUE,
                                        plotOutput("grafico_ACF", height = 500)
                                    )
                             )
                           )
                         )
                ),
                tabPanel("Mortalidade", 
                         conditionalPanel(
                           condition = "input.tabs == 'Mortalidade'",
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
                             column(width = 6,
                                    box(width = NULL, solidHeader = TRUE,
                                        plotlyOutput("grafico_series_estac1", height = 500)
                                    )
                             ),
                             
                             column(width = 6,
                                    box(width = NULL, solidHeader = TRUE,
                                        plotOutput("grafico_ACF1", height = 500)
                                    )
                             )
                           )
                         )
                ),
                tabPanel("Vacinas", 
                         conditionalPanel(
                           condition = "input.tabs == 'Vacinas'",
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
                           ),
                           
                           fluidRow(
                             column(width = 6,
                                    box(width = NULL, solidHeader = TRUE,
                                        plotlyOutput("grafico_series_estac2", height = 500)
                                    )
                             ),
                             
                             column(width = 6,
                                    box(width = NULL, solidHeader = TRUE,
                                        plotOutput("grafico_ACF2", height = 500)
                                    )
                             )
                           )
                         )
                )
                # Adicione mais abas conforme necessário, sem vírgula após a última
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

# Definir o servidor
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
  
#------------------------------------------------------------
  
  output$grafico_series <- renderPlotly({
    render_grafico_series(input, "confirmed")
  })
  
  output$grafico_saz <- renderPlotly({
    grafico_sazonalidade_casos(input,output)
  })
  
  output$grafico_series_estac <- renderPlotly({
    render_grafico_series_estacionaria(input, "confirmed")
  })
  
  output$grafico_ACF <- renderPlot({
    render_grafico_ACF(input, "confirmed")
  })

#------------------------------------------------------------
  
  output$grafico_series1 <- renderPlotly({
    render_grafico_series(input, "deaths")
  })
  
  
  output$grafico_saz1 <- renderPlotly({
    grafico_sazonalidade_mortes(input,output)
  })
  
  output$grafico_series_estac1 <- renderPlotly({
    render_grafico_series_estacionaria(input, "deaths")
  })
  
  output$grafico_ACF1 <- renderPlot({
    render_grafico_ACF(input, "deaths")
  })
  
#------------------------------------------------------------
  
  output$grafico_series2 <- renderPlotly({
    render_grafico_series(input, "vaccines", 10000)
  })
  
  output$grafico_saz2 <- renderPlotly({
    grafico_sazonalidade_vacinas(input,output)
  })
  
  output$grafico_series_estac2 <- renderPlotly({
    render_grafico_series_estacionaria(input, "vaccines", 10000)
  })
  
  output$grafico_ACF2 <- renderPlot({
    render_grafico_ACF(input, "vaccines", 10000)
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