# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard, 
               shinyWidgets, 
               plotly)

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
Vars_nomes <- c("Casos Confirmados", "Óbitos", "Vacinas Administradas")

#Períodos
Periodos <- c("year","week")
Periodos_nomes <- c("Anual","Semanal")

# Criar lista de medidas políticas
m_p <- c("cancel_events", "elderly_people_protection", "facial_coverings", "gatherings_restrictions",
         "information_campaigns", "school_closing", "stay_home_restrictions", "testing_policy",
         "transport_closing", "vaccination_policy", "workplace_closing")

m_p_nomes <- c("Cancelamento de Eventos", "Proteção a Idosos", "Uso de Máscaras", "Restrições em Aglomerações",
               "Campanhas Informativas", "Fechamento de Escolas", "Restrições de Permanência em Casa",
               "Política de Testagem", "Fechamento de Transporte", "Vacinação", "Fechamento de Locais de Trabalho")

indices <- c("government_response_index", "stringency_index", "containment_health_index", "economic_support_index")

indices_nomes <- c("Índice de resposta do governo", "Índice de rigor das políticas de restrição",
                   "Índice de medidas de contenção e saúde", "Índice de medidas de suporte econômico")

#------------------------------------------------------------

# Definir o UI
ui <- dashboardPage(
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
    uiOutput("style"),
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
                           selectInput("var", "Variável", setNames(Vars, Vars_nomes), selectize = TRUE),
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
                       box(title = span(icon("chart-line"), " Gráfico de séries com tendencia estimada"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           tabBox(
                             title = NULL,
                             id = "tabset1",
                             width = NULL,
                             tabPanel("Casos Diários",
                                      plotlyOutput("grafico_series", height = 500)
                             ),
                             tabPanel("Casos Acumulados",
                                      plotlyOutput("grafico_series_acumulada", height = 500)
                             )
                           ),
                           collapsible = TRUE, collapsed = FALSE
                       )
                )
              ),
              
              
              fluidRow(
                column(width = 12,
                       box(title = span(icon("chart-line"), " Componentes da série"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           fluidRow(column(width = 6,
                                           dropdown(
                                             tags$h3("Selecione o período de sua preferência"),
                                             selectInput(
                                               "periodo",
                                               label = NULL,
                                               choices = setNames(Periodos, Periodos_nomes),
                                               selectize = TRUE
                                             ),
                                             style = "unite", icon = icon("gear"),
                                             status = "primary", width = "300px",
                                             tooltip = tooltipOptions(title = "Clique para ver mais filtros!")),
                                           plotlyOutput("grafico_saz", height = 500)),
                                    column(width = 6,
                                           plotlyOutput("grafico_transf1", height = 500))),
                           fluidRow(column(width = 6,
                                           plotlyOutput("grafico_transf2", height = 500)),
                                    column(width = 6,
                                           plotlyOutput("grafico_ACF_transf2", height = 500))),
                           collapsible = TRUE, collapsed = TRUE
                       )
                )
              ),
              
              fluidRow(
                column(width = 12,
                       box(title = span(icon("circle-question"), " Informaçoes sobre a Interpretação"),
                           width = NULL, status = "success", solidHeader = TRUE,
                           # htmlOutput("texto_sobre"),
                           collapsible = TRUE, collapsed = TRUE
                       ),
                )
              )
              
      ),
      
      
      
      tabItem("dg",
              fluidRow(
                column(width = 4, 
                       box(title = span(icon("location-dot"), " Selecione a região de sua preferência"), 
                           width = NULL, status = "info", solidHeader = TRUE,
                           column(width = 6,
                                  selectInput("e_c1", "Estado1", est, selectize = TRUE),
                                  uiOutput('html_filtro_cidade1'), collapsible = TRUE),
                           column(width = 6,
                                  selectInput("e_c2", "Estado2", est, selectize = TRUE),
                                  uiOutput('html_filtro_cidade2'), collapsible = TRUE)
                       ),
                ),
                
                column(width = 4,
                       box(title = span(icon("x"), " Selecione a variável de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("var1", "Variável", setNames(Vars, Vars_nomes), selectize = TRUE),
                           collapsible = TRUE
                       )
                ),
                
                column(width = 4,
                       box(title = span(icon("calendar"), " Selecione o período de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
                           sliderInput("date_slider1", "Período", min = min(dados_pais$date), max = max(dados_pais$date),
                                       value = c(min(dados_pais$date), max(dados_pais$date)))
                       )
                )
              ),
              
              fluidRow(
                column(width = 12, 
                       box(title = span(icon("chart-line"), " Gráfico de séries entre duas regiões"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           plotlyOutput("grafico_series_1e2", height = 500),
                           collapsible = TRUE, collapsed = FALSE
                       )
                )
              )
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
                           selectInput("var2", "Variável", setNames(Vars, Vars_nomes), selectize = TRUE),
                           selectInput("med_pol", "Medidas Políticas", setNames(m_p, m_p_nomes), selectize = TRUE),
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
                       box(title = span(icon("chart-line"), " Gráfico de séries para Medidas Políticas"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           plotlyOutput("grafico_series_med_pol", height = 500),
                           collapsible = TRUE, collapsed = FALSE
                       )
                )
              )
              
      ),
      
      
      
      tabItem("ind",
              fluidRow(
                column(width = 4,
                       box(title = span(icon("location-dot"), " Selecione a região de sua preferência"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("e_c_ind", "Estado", est, selectize = TRUE)
                           ,uiOutput('html_filtro_cidade_ind'), collapsible = TRUE
                       )
                ),
                
                column(width = 4,
                       box(title = span(icon("landmark"), " Selecione a variável e o índice"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           selectInput("var3", "Variável", setNames(Vars, Vars_nomes), selectize = TRUE),
                           selectInput("ind_pol", "Índices", setNames(indices, indices_nomes), selectize = TRUE),
                           collapsible = TRUE
                       )
                ),
                
                column(
                  width = 4,
                  box(title = span(icon("calendar"), " Selecione o período de sua preferência"),
                      width = NULL, status = "info", solidHeader = TRUE,
                      sliderInput("date_slider_ind", "Período", min = min(dados_pais$date), max = max(dados_pais$date),
                                  value = c(min(dados_pais$date), max(dados_pais$date))),
                      collapsible = TRUE
                  )
                )
                
              ),
              
              fluidRow(
                column(width = 12, 
                       box(title = span(icon("chart-line"), " Gráfico de séries para Índices Políticos"),
                           width = NULL, status = "info", solidHeader = TRUE,
                           plotlyOutput("grafico_series_ind_pol", height = 500),
                           collapsible = TRUE, collapsed = FALSE
                       )
                )
              )
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
      )
    )
  )
)