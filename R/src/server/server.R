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
    render_grafico_series(input)
  })
  
  output$grafico_series_acumulada <- renderPlotly({
    render_grafico_series_acumulada(input)
  })
  
  output$grafico_saz <- renderPlotly({
    render_grafico_sazonalidade(input)
  })
  
  output$grafico_series_estac <- renderPlotly({
    render_grafico_series_estacionaria(input, saz=T)
  })
  
  output$grafico_estac <- renderPlotly({
    render_grafico_series_estacionaria(input)
  })
  
  output$grafico_ACF <- renderPlotly({
    render_grafico_ACF(input)
  })
  
  #------------------------------------------------------------

  output$grafico_transf1 <- renderPlotly({
    render_grafico_series_estacionaria(input, transf = 1)
  })

  output$grafico_ACF_transf1 <- renderPlotly({
    render_grafico_ACF(input, transf = 1)
  })
  
  output$grafico_transf2 <- renderPlotly({
    render_grafico_series_estacionaria(input, transf = 2)
  })

  output$grafico_ACF_transf2 <- renderPlotly({
    render_grafico_ACF(input, transf = 2)
  })
  
  output$grafico_transf3 <- renderPlotly({
    render_grafico_series_estacionaria(input, transf = 3)
  })
  
  output$grafico_ACF_transf3 <- renderPlotly({
    render_grafico_ACF(input, transf = 3)
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
    render_grafico_series_comp_geo(input)
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
  
  # CSS para atualizar o slider
  output$style <- renderUI({
    tags$style(HTML(
      "
      /* Estilo para o slider*/
      .irs--flat .irs-bar {
      top: 25px;
      height: 12px;
      background-color: #00C0EF;
    }
    .irs--flat .irs-from, .irs--flat .irs-to, .irs--flat .irs-single {
      color: white;
      font-size: 10px;
      line-height: 1.333;
      text-shadow: none;
      padding: 1px 5px;
      background-color: #00C0EF;
      border-radius: 4px;
    }
    .irs--flat .irs-handle>i:first-child {
      position: absolute;
      display: block;
      top: 0;
      left: 50%;
      width: 2px;
      height: 100%;
      margin-left: -1px;
      background-color: #00C0EF;
    }
    .irs--flat .irs-from:before, .irs--flat .irs-to:before, .irs--flat .irs-single:before {
      position: absolute;
      display: block;
      content: '';
      bottom: -6px;
      left: 50%;
      width: 0;
      height: 0;
      margin-left: -3px;
      overflow: hidden;
      border: 3px solid transparent;
      border-top-color: #00C0EF; 
    }
    
    /* Estilo para a aba ativa no tabBox */
    .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #00C0EF;
    }
    
    /* Estilo caixa de informaçoes dos graficos */
    .box.box-solid.box-success>.box-header {
    color: #fff;
    background: #7c878b;
    background-color: #7c878b;
    }
    .box.box-solid.box-success {
    border: 1px solid #7c878b;
    }


      "
    ))
  })
  
}