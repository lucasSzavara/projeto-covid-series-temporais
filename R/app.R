# Pacotes
#install.packages("pacman")
pacman::p_load(shiny, shinydashboard, shinythemes, COVID19, fpp3, plotly, forecast)

#------------------------------------------------------------

# Dados para teste

# dados <- covid19(country = c('Brazil'), level=2, verbose=F)
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

corrige <- function(df,variavel){
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
              column(width = 2, 
                     box(width = NULL, status = "warning", solidHeader = TRUE,
                         selectInput("e_c", "Estado", est, selectize = TRUE)),
                     uiOutput('html_filtro_cidade'),
                     # box(width = NULL, status = "warning", solidHeader = TRUE,
                     #     sliderInput("date_slider", "Período", min = min(dados$date), max = max(dados$date), 
                     #                 value = c(min(dados$date), max(dados$date)))),
                     # box(width = NULL, status = "warning", solidHeader = TRUE,
                     #     selectInput("inicio", "Data de início do gráfico", inicio))),
              ),
              column(width = 10, 
                     box(width = NULL, solidHeader = TRUE, 
                         plotlyOutput("grafico_series", height = 500)))
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
server <- function(input, output) {
  
  # Lógica ou ações específicas para cada página podem ser adicionadas aqui
  
  output$html_filtro_cidade <- renderUI({
    est <- input$estado_filtro
    if(!(is.null(est) || est == '')) {
      estados_cidades <- read.csv('estados_cidades.csv')
      cidades <- unique(estados_cidades[estados_cidades$estados == est, ]$cidades)
      print(length(cidades))
      print(est)
      return(box(width = NULL, status = "warning", solidHeader = TRUE,
          selectInput("cidade_filtro", "Cidade", c('', cidades), selectize = TRUE)))
    }
  })
  output$grafico_series <- renderPlotly({
    est <- input$estado_filtro
    cid <- input$cidade_filtro
    if(is.null(est) || est == '') {
      df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F) # %>% filter(date >= input$date_slider[1],
                                                        #           date <= input$date_slider[2])
    } else {
      if(!(is.null(cid) || cid == '')) {
        df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv'))
      }
      else {
        df_cidade <- covid19(country = c('Brazil'), level=2, verbose=F) %>% filter(administrative_area_level_2 == est)
      }
    }
    
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
}

#------------------------------------------------------------

# Crie o aplicativo Shiny
shinyApp(ui, server)