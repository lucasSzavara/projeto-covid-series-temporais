# Carregar Pacotes
pacman::p_load(shiny, 
               shinydashboard)

#------------------------------------------------------------

# Carregar Funções
source("./src/services/index.R")
source("./src/server/graficos/index.R")
source("./tendencia.R")
source('./sazonalidade.R')

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
    render_grafico_series_estacionaria(input, "confirmed", saz=T)
  })
  
  output$grafico_estac <- renderPlotly({
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
  
  #================================================================== Efeito de Medidas Políticas
  
  output$html_filtro_cidade_mp <- renderUI({
    est <- input$e_c_mp
    if(!(is.null(est) || est == '')) {
      cidades <- sort(unique(locais[locais$estados == est, ]$cidades))
      return(selectInput("cidade_filtro_mp", "Cidade", c('', cidades), selectize = TRUE))
    }
  })
  observe({
    est <- input$e_c_mp
    updateSelectInput(session, 'cidade_filtro_mp', selected='')
  })
  
  #------------------------------------------------------------
  
  output$grafico_series_med_pol <- renderPlotly({
    render_grafico_series_mp(input)
  })
  
  
  #------------------------------------------------------------
  
  #================================================================== END: Efeito de Medidas Políticas
  
  
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
      tags$p("1.", tags$a("Evolução", onclick = "openTab('evo')", href = "#"), 
             "- Nesta seção, propomos uma análise temporal das variáveis fundamentais relacionadas à COVID-19. 
             Explore a dinâmica de casos confirmados, óbitos e administração de vacinas, observando as tendências ao longo do tempo."),
      tags$p("2.", tags$a("Diferenças Geográficas", onclick = "openTab('dg')", href = "#"), 
             "- Nesta seção, convidamos você a explorar as nuances das variáveis essenciais relacionadas à COVID-19 em diferentes regiões do Brasil. 
             A análise se concentra na comparação entre estados e cidades, fornecendo insights valiosos sobre padrões regionais."),
      tags$p("3.", tags$a("Efeito de Medidas Políticas", onclick = "openTab('efeito')", href = "#"), 
             "- Analise o impacto das medidas políticas na propagação da COVID-19, 
             avaliando como as ações governamentais influenciam os indicadores epidemiológicos."),
      tags$p("4.", tags$a("Índices", onclick = "openTab('ind')", href = "#"), 
             "- Explore índices específicos, como o Índice de Resposta do Governo, o Índice de Rigor das Políticas de Restrição, 
             o Índice de Medidas de Contenção e Saúde, e o Índice de Medidas de Suporte Econômico. Esses indicadores fornecem uma compreensão mais holística da situação, 
             abrangendo desde a eficácia das ações governamentais até o suporte econômico implementado."),
      
      tags$p(tags$b("Código no GitHub:")),
      tags$p("O código-fonte deste projeto está disponível no GitHub. Você pode acessá-lo em:",
             tags$a(href = 'https://github.com/lucasSzavara/projeto-covid-series-temporais', target = "_blank", "GitHub Repository")),
      
      tags$p("Para mais informações sobre cada aba, consulte a seção correspondente.")
    )
  })
  
  
  
}