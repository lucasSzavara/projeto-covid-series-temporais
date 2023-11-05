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

# Variáveis
Vars <- c("confirmed","deaths","vaccines")

# Criar lista de medidas políticas
m_p <- c("school_closing","workplace_closing","cancel_events","gatherings_restrictions",
         "transport_closing","stay_home_restrictions","information_campaigns","testing_policy",
         "elderly_people_protection","facial_coverings","vaccination_policy")

#------------------------------------------------------------

# Definir o UI
ui <- dashboardPage(
  # ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = span(icon("viruses"), "Covid-19")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Evolução", tabName = "evo", icon = icon("chart-line")),
      menuItem("Diferenças geográficas", tabName = "dg", icon = icon("arrow-down-up-across-line")),
      menuItem("Efeito de medidas políticas", tabName = "efeito",icon = icon("landmark")),
      menuItem("Índices", tabName = "ind",icon = icon("signal")),
      menuItem("Sobre", tabName = "sobre",icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    setSliderColor(c("#00C0EF", "#00C0EF", "#00C0EF"), c(1, 2, 3)),  # Aplica a cor aos três sliders
    chooseSliderSkin("Flat"),
    tabItems(
      tabItem(tabName = "evo",
              fluidRow(
                column(width = 4,
                       box(title = span(icon("location-dot"), " Selecione a região de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("e_c", "Estado", est, selectize = TRUE)
                           ,uiOutput('html_filtro_cidade'), collapsible = TRUE
                       )
                ),
                
                column(width = 4,
                       box(title = span(icon("x"), " Selecione a variável de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("var", "Variável", Vars, selectize = TRUE),
                           collapsible = TRUE
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
              
              fluidRow(
                column(width = 12,
                       box(title = span(icon("chart-line"), " Gráfico de series com tendencia estimada"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           plotlyOutput("grafico_series", height = 500),
                           collapsible = TRUE, collapsed = FALSE
                       )
                )
              ),
              
              fluidRow(
                column(width = 12,
                       box(title = span(icon("chart-line"), " Componentes da serie"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           fluidRow(column(width = 6,
                                           plotlyOutput("grafico_saz", height = 500)),
                             column(width = 6,
                                    plotlyOutput("grafico_series_estac", height = 500))),
                           fluidRow(column(width = 6,
                                           plotlyOutput("grafico_estac", height = 500)),
                                    column(width = 6,
                                           plotlyOutput("grafico_ACF", height = 500))),
                           collapsible = TRUE, collapsed = TRUE
                       )
                )
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
              fluidRow(
                column(width = 4,
                       box(title = span(icon("location-dot"), " Selecione a região de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("e_c_mp", "Estado", est, selectize = TRUE)
                           ,uiOutput('html_filtro_cidade_mp'), collapsible = TRUE
                       )
                ),
                
                column(width = 4,
                       box(title = span(icon("landmark"), " Selecione a variável e a medida política"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("var2", "Variável", Vars, selectize = TRUE),
                           selectInput("med_pol", "Medidas Políticas", m_p, selectize = TRUE),
                           collapsible = TRUE
                       )
                ),
                
                column(
                  width = 4,
                  box(title = span(icon("calendar"), " Selecione o período de sua preferência"),
                      width = NULL, status = "info", solidHeader = TRUE,
                      sliderInput("date_slider_mp", "Período", min = min(dados_pais$date), max = max(dados_pais$date),
                                  value = c(min(dados_pais$date), max(dados_pais$date))),
                      collapsible = TRUE
                  )
                )
                
              ),
              
              fluidRow(
                column(width = 12,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("grafico_series_med_pol", height = 500)
                       )
                )
                
              )
              
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