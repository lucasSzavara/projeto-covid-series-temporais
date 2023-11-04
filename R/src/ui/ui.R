# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard, 
               shinyWidgets, 
               plotly)

# Função para personalizar a cor dos sliders
# https://github.com/dreamRs/shinyWidgets/blob/26838f9e9ccdc90a47178b45318d110f5812d6e1/R/setSliderColor.R
setSliderColor <- function(color, sliderId) {
  # some tests to control inputs
  stopifnot(!is.null(color))
  stopifnot(is.character(color))
  stopifnot(is.numeric(sliderId))
  stopifnot(!is.null(sliderId))
  
  # the css class for ionrangeslider starts from 0
  # therefore need to remove 1 from sliderId
  sliderId <- sliderId - 1
  
  # create custom css background for each slider
  # selected by the user
  sliderCol <- lapply(sliderId, FUN = function(i) {
    paste0(
      ".js-irs-", i, " .irs-single,",
      " .js-irs-", i, " .irs-from,",
      " .js-irs-", i, " .irs-to,",
      " .js-irs-", i, " .irs-bar-edge,",
      " .js-irs-", i,
      " .irs-bar{  border-color: transparent;background: ", color[i+1],
      "; border-top: 1px solid ", color[i+1],
      "; border-bottom: 1px solid ", color[i+1],
      ";}"
    )
  })
  
  # insert this custom css code in the head
  # of the shiny app
  custom_head <- tags$head(tags$style(HTML(as.character(sliderCol))))
  return(custom_head)
}

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
  dashboardHeader(title = span(icon("viruses"), "Covid-19")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "vg", icon = icon("magnifying-glass")),
      menuItem("Evolução", tabName = "evo", icon = icon("chart-line")),
      menuItem("Diferenças geográficas", tabName = "dg", icon = icon("arrow-down-up-across-line")),
      menuItem("Efeito de medidas políticas", tabName = "efeito",icon = icon("landmark")),
      menuItem("Índices", tabName = "ind",icon = icon("signal")),
      menuItem("Sobre", tabName = "sobre",icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    setSliderColor(c("#00C0EF", "#00C0EF"), c(1, 2)),  # Aplica a cor aos dois sliders
    chooseSliderSkin("Flat"),
    tabItems(
      tabItem(tabName = "vg",
              h2("Visão Geral"),
              p("Menu sobre covid-19 no Brasil")
              # Adicione elementos específicos para a Página 1 aqui
      ),
      tabItem(tabName = "evo",
              fluidRow(
                column(width = 4,
                       box(title = span(icon("location-dot"), " Selecione a região de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("e_c", "Estado", est, selectize = TRUE)
                           ,uiOutput('html_filtro_cidade'), collapsible = TRUE
                       )
                ),
                
                column(
                  width = 4,
                  box(title = span(icon("calendar"), " Selecione o período de sua preferência"),
                      width = NULL, status = "info", solidHeader = TRUE,
                      sliderInput("date_slider", "Período", min = min(dados_pais$date), max = max(dados_pais$date),
                                  value = c(min(dados_pais$date), max(dados_pais$date))),
                      collapsible = TRUE
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
                                        plotlyOutput("grafico_ACF", height = 500)
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
                                        plotlyOutput("grafico_ACF1", height = 500)
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
                                        plotlyOutput("grafico_ACF2", height = 500)
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
                       box(title = span(icon("location-dot"), " Selecione a região de sua preferência"), 
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("e_c1", "Estado1", est, selectize = TRUE),
                           uiOutput('html_filtro_cidade1'), collapsible = TRUE
                       ),
                ),
                
                column(width = 4,
                       box(title = span(icon("location-dot"), " Selecione a região de sua preferência"), 
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("e_c2", "Estado2", est, selectize = TRUE),
                           uiOutput('html_filtro_cidade2'), collapsible = TRUE
                       ),
                ),
                
                column(width = 4,
                       box(title = span(icon("calendar"), " Selecione o período de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
                           sliderInput("date_slider1", "Período", min = min(dados_pais$date), max = max(dados_pais$date),
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
      ),
      tabItem("sobre",
              # h2("texto"),
              fluidRow(
                column(width = 12, 
                       box(title = span(icon("circle-info"), " Informaçoes sobre o dashboard"), 
                           width = NULL, status = "info", solidHeader = TRUE,
                           htmlOutput("texto_sobre")
                       ),
                )
              )
              # Adicione elementos específicos para a Página 5 aqui
      )
    )
  )
)
