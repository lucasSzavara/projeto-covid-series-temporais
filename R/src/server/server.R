# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard)

#------------------------------------------------------------

# Carregar Funções
source("./src/funcoes auxiliares/index.R")
source("./src/server/graficos/index.R")

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