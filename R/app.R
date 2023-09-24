# Pacotes
#install.packages("pacman")
pacman::p_load(shiny, shinydashboard, shinythemes, COVID19, fpp3, plotly, forecast)

#------------------------------------------------------------

# Dados para teste

dados <- read.csv("df_capitais.csv")
dados$date <- as.Date(dados$date, format = "%Y-%m-%d")
# Filtragem dos dados
df_confirmed <- dados[,c("id","date","confirmed","administrative_area_level_2","administrative_area_level_3")]
df_confirmed <- na.omit(df_confirmed)

df_confirmed$estado_cidade <- paste(df_confirmed$administrative_area_level_2, df_confirmed$administrative_area_level_3, sep = "/")

#------------------------------------------------------------

# Selecionadores

est_city <- unique(df_confirmed$estado_cidade)

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
      menuItem("Visualizações", tabName = "vis", icon = icon("th")),
      menuItem("Evolução", tabName = "ev", icon = icon("chart-line")),
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
                         selectInput("e_c", "Estado/Cidade", est_city)),
                     box(width = NULL, status = "warning", solidHeader = TRUE,
                         sliderInput("date_slider", "Período", min = min(dados$date), max = max(dados$date), 
                                     value = c(min(dados$date), max(dados$date)))),
                     box(width = NULL, status = "warning", solidHeader = TRUE,
                         selectInput("inicio", "Data de início do gráfico", inicio))),
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
  output$grafico_series <- renderPlotly({
    state_city <- input$e_c
    df_cidade <- df_confirmed %>%
      filter(estado_cidade == state_city,
             date >= input$date_slider[1],
             date <= input$date_slider[2])
    
    df_cidade <- corrige(df_cidade,"confirmed")
    
    p <- df_cidade %>%
      slice(-1) %>%
      mutate(confirmed = diff(df_cidade$confirmed)) %>%
      ggplot(aes(x = date, y = confirmed)) +
      geom_line(color = "blue") +
      labs(title = paste("Casos confirmados em",state_city), x = "Data", y = "Novos Confirmados Diários") +
      theme_minimal() +
      scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
    
    fig <- ggplotly(p)
    fig
  })
}

#------------------------------------------------------------

# Crie o aplicativo Shiny
shinyApp(ui, server)