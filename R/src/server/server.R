# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard)

#------------------------------------------------------------

# Carregar Funções
source("R/src/services/index.R")
source("R/src/server/graficos/index.R")
source("R/tendencia.R")

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
    render_grafico_sazonalidade(input, "confirmed")
  })
  
  output$grafico_series_estac <- renderPlotly({
    render_grafico_series_estacionaria(input, "confirmed")
  })
  
  output$grafico_ACF <- renderPlotly({
    render_grafico_ACF(input, "confirmed")
  })
  
  #------------------------------------------------------------
  
  output$grafico_series1 <- renderPlotly({
    render_grafico_series(input, "deaths")
  })
  
  
  output$grafico_saz1 <- renderPlotly({
    render_grafico_sazonalidade(input, "deaths")
  })
  
  output$grafico_series_estac1 <- renderPlotly({
    render_grafico_series_estacionaria(input, "deaths")
  })
  
  output$grafico_ACF1 <- renderPlotly({
    render_grafico_ACF(input, "deaths")
  })
  
  #------------------------------------------------------------
  
  output$grafico_series2 <- renderPlotly({
    render_grafico_series(input, "vaccines", 10000)
  })
  
  output$grafico_saz2 <- renderPlotly({
    render_grafico_sazonalidade(input, "vaccines", 10000)
  })
  
  output$grafico_series_estac2 <- renderPlotly({
    render_grafico_series_estacionaria(input, "vaccines", 10000)
  })
  
  output$grafico_ACF2 <- renderPlotly({
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
    render_grafico_series_comp_geo(input,"vaccines",10000)
  })
  
  #================================================================== END: DIFERENÇAS GEOGRÁFICAS
  output$texto_sobre <- renderUI({
    tagList(
      tags$p(tags$b("Proposta do Dashboard:")),
      tags$p("Este dashboard tem como objetivo fornecer uma visão abrangente dos dados relacionados à COVID-19."),
      
      tags$p(tags$b("Origem dos Dados:")),
      tags$p("Os dados utilizados são provenientes do",
             tags$a(href = 'https://covid19datahub.io', target = "_blank", "COVID-19 Data Hub"), "."),
      
      tags$p(tags$b("Modelos Utilizados:")),
      tags$p("Descrever brevemente os modelos."),
      
      tags$p(tags$b("Navegação no Dashboard:")),
      tags$p("Clique nas abas abaixo para explorar diferentes aspectos dos dados:"),
      tags$p("1.", tags$a("Visão Geral", onclick = "openTab('vg')", href = "#"), 
             "- X"),
      tags$p("2.", tags$a("Evolução", onclick = "openTab('evo')", href = "#"), 
             "- Nesta seção, propomos uma análise temporal das variáveis fundamentais relacionadas à COVID-19. 
             Explore a dinâmica de casos confirmados, óbitos e administração de vacinas, observando as tendências ao longo do tempo."),
      tags$p("3.", tags$a("Diferenças Geográficas", onclick = "openTab('dg')", href = "#"), 
             "- Nesta seção, convidamos você a explorar as nuances das variáveis essenciais relacionadas à COVID-19 em diferentes regiões do Brasil. 
             A análise se concentra na comparação entre estados e cidades, fornecendo insights valiosos sobre padrões regionais."),
      tags$p("4.", tags$a("Efeito de Medidas Políticas", onclick = "openTab('efeito')", href = "#"), 
             "- X"),
      tags$p("5.", tags$a("Índices", onclick = "openTab('ind')", href = "#"), 
             "- X"),
      tags$p("6.", tags$a("Sobre", onclick = "openTab('sobre')", href = "#"), 
             "- X"),
      
      tags$p(tags$b("Código no GitHub:")),
      tags$p("O código-fonte deste projeto está disponível no GitHub. Você pode acessá-lo em:",
             tags$a(href = 'https://github.com/lucasSzavara/projeto-covid-series-temporais', target = "_blank", "GitHub Repository")),
      
      tags$p("Para mais informações sobre cada aba, consulte a seção correspondente.")
    )
  })
  
  
  
}