# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard,
               plotly)

#------------------------------------------------------------

# Carregar lista de nomes de estados e respectivas cidades
locais <- read.csv('dados/estados_cidades.csv')
dados_pais <- read.csv('dados/pais/dados_pais.csv')
dados_pais$date <- as.Date(dados_pais$date)

#------------------------------------------------------------

# Criar lista de estados
est <- c('', sort(unique(locais$estados)))

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
                           sliderInput("date_slider", "Período", min = min(dados_pais$date), max = max(dados_pais$date),
                                       value = c(min(dados_pais$date), max(dados_pais$date)))
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
                           sliderInput("data_slider1", "Período", min = min(dados_pais$date), max = max(dados_pais$date),
                                       value = c(min(dados_pais$date), max(dados_pais$date)))
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
