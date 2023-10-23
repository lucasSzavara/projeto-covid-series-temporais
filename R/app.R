# Pacotes
# install.packages("pacman")
pacman::p_load(shiny, 
               shinydashboard, 
               shinythemes, 
               COVID19, 
               fpp3, 
               plotly, 
               forecast, 
               stringr, 
               drc)


#------------------------------------------------------------

# Dados para teste

dados_estados <- covid19(country = c('Brazil'), level=2, verbose=F)
# dados$date <- as.Date(dados$date, format = "%Y-%m-%d")
locais <- read.csv('estados_cidades.csv')
# Filtragem dos dados
# df_confirmed <- dados[,c("id","date","confirmed","administrative_area_level_2","administrative_area_level_3")]
# df_confirmed <- na.omit(df_confirmed)
#------------------------------------------------------------

# Selecionadores


est <- c('', sort(unique(locais$estados)))

inicio <- c("Analisar apartir de X confirmados")

#------------------------------------------------------------

# Funções

CubicInterpSplineAsPiecePoly <- function (x, y, method = c("fmm", "natural", "periodic", "hyman")) {
  ## method validation
  if (!(method %in% c("fmm", "natural", "periodic", "hyman")))
    mystop("'method' must be one of the following: 'fmm', 'natural', 'periodic', 'hyman'!")
  ## use `splinefun` for cubic spline interpolation
  CubicInterpSpline <- stats::splinefun(x, y, method)
  ## extract construction info
  construction_info <- environment(CubicInterpSpline)$z
  ## export as an "PiecePoly" object
  pieces <- seq_len(length(construction_info$x) - 1L)
  PiecePolyCoef <- with(construction_info, rbind(y[pieces], b[pieces], c[pieces], d[pieces], deparse.level = 0L))
  structure(list(PiecePoly = list(coef = PiecePolyCoef, shift = TRUE),
                 knots = construction_info$x), method = method,
            class = c("PiecePoly", "CubicInterpSpline"))
}

## represent a fitted smoothing spline as an interpolation spline
SmoothSplineAsPiecePoly <- function (SmoothSpline) {
  ## input validation
  if (!inherits(SmoothSpline, "smooth.spline"))
    mystop("This function only works with models that inherit 'smooth.spline' class!")
  ## knots of the smoothing spline
  kx <- with(SmoothSpline$fit, knot * range + min)
  kx <- kx[4:(length(kx) - 3)]
  ky <- predict(SmoothSpline, kx, 0L)[[2]]  ## deriv = 0L
  ## natural cubic spline interpolation over the knots
  CubicInterpSplineAsPiecePoly(kx, ky, method = "natural")
}

predict.PiecePoly <- function (object, newx, deriv = 0L, ...) {
  ## change symbol
  PiecePolyObject <- object
  ## extract piecewise polynomial coefficients
  PiecePolyCoef <- PiecePolyObject$PiecePoly$coef
  shift <- PiecePolyObject$PiecePoly$shift
  ## get degree
  degree <- dim(PiecePolyCoef)[1L] - 1L
  ## deriv validation
  if (deriv > degree) return(numeric(length(newx)))
  ## get power
  power <- 0:(degree - deriv)
  ## extract knots
  x <- PiecePolyObject$knots
  ## which piece?
  piece_id <- findInterval(newx, x, TRUE)
  ind <- split.default(seq_len(length(newx)), piece_id)
  unique_piece_id <- as.integer(names(ind))
  n_pieces <- length(unique_piece_id)
  ## loop through pieces
  y <- numeric(length(newx))
  i <- 1L
  while (i <= n_pieces) {
    ii <- unique_piece_id[i]
    xi <- newx[ind[[i]]] - shift * x[ii]
    pc <- PiecePolyCoef[, ii]
    if (deriv > 0) pc <- pc[-seq_len(deriv)] * choose(deriv:degree, deriv) * factorial(deriv)
    y[ind[[i]]] <- c(outer(xi, power, "^") %*% pc)
    i <- i + 1L
  }
  y
}

## `solve` method for "PiecePoly"
##    solve
##    function (a, b, ...)
##  1. backsolve 'x' value given a 'y' value on the spline
##  2. find extrema of the spline
solve.PiecePoly <- function (a, b = 0, deriv = 0L, ...) {
  ## change symbol
  PiecePolyObject <- a
  y <- b
  ## helpful message (in case someone used `y = y0` than `b = y0` to give RHS which returns misleading results)
  cat(sprintf("solving equation for RHS value %.7g\n", y))
  ## extract piecewise polynomial coefficients
  PiecePolyCoef <- PiecePolyObject$PiecePoly$coef
  shift <- PiecePolyObject$PiecePoly$shift
  n_pieces <- dim(PiecePolyCoef)[2L]
  ## get degree
  degree <- dim(PiecePolyCoef)[1L] - 1L
  ## extract knots
  x <- PiecePolyObject$knots
  ## deriv validation
  if (deriv >= degree) mystop("'deriv' can not exceed 'degree'!")
  ## list of roots on each piece
  xr <- vector("list", n_pieces)
  ## loop through pieces
  i <- 1L
  while (i <= n_pieces) {
    ## polynomial coefficient
    pc <- PiecePolyCoef[, i]
    ## take derivative
    if (deriv > 0) pc <- pc[-seq_len(deriv)] * choose(deriv:degree, deriv) * factorial(deriv)
    pc[1] <- pc[1] - y
    ## complex roots
    croots <- base::polyroot(pc)
    ## real roots (be careful when testing 0 for floating point numbers)
    rroots <- Re(croots)[round(Im(croots), 10) == 0]
    ## is shifting needed?
    if (shift) rroots <- rroots + x[i]
    ## real roots in (x[i], x[i + 1])
    xr[[i]] <- rroots[(rroots >= x[i]) & (rroots <= x[i + 1])]
    ## next piece
    i <- i + 1L
  }
  ## collapse list to atomic vector and return
  unlist(xr)
}

corrige <- function(df, variavel){
  df[is.na(df)] <- 0
  for(i in 2:length(df[[variavel]])){
    if(df[i,variavel] < df[i-1,variavel]){
      df[i,variavel] <- df[i-1,variavel]
    }
  }
  return(df)
}

grafico_series <- function(datas, series, titulo_grafico, eixo_x, eixo_y) {
  tendencias <- estima_tendencia(series)
  p <- as.data.frame(cbind(serie=series)) %>%
    slice(-1) %>%
    mutate(serie_mutada = diff(series)) %>%
    ggplot(aes(x = datas[2:length(datas)], y = serie_mutada)) +
    geom_line(color = "blue") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y") +
    geom_line(aes(x=datas[2:length(datas)], y=diff(tendencias)), color='red')
  
  fig <- ggplotly(p)
  return(fig)
  
}

grafico_sazonal <- function(datas, serie, titulo_grafico, eixo_x, eixo_y, periodo, verbose=F) {
  dados = tsibble(
    data = datas,
    y = serie,
    index = data
  )
  
  dados <- dados %>%
    fill_gaps(data, .full=FALSE)
  # Plotando a série temporal
  G =
    dados %>%
    gg_season(y, period=periodo) +
    labs(
      y = eixo_y,
      x = eixo_x,
      title = titulo_grafico,
      labels = "right"
    ) +
    guides(color = guide_legend(title = "Legenda"))
  interactive_plot <- ggplotly(G, tooltip = c("all"))
  interactive_plot <- interactive_plot %>%
    layout(showlegend = T)
  fig <- plotly_build(interactive_plot)
  hover <- c()
  for(i in 1:length(fig$x$data)){
    if(verbose) {
      print(fig$x$data[[i]]$text)
    }
    fig$x$data[[i]]$hovertemplate <- c(hover, paste('<b>', eixo_y, '</b>: %{y:,.0f}',
                                                    '<b>Data</b>', datas[year(datas)==str_sub(fig$x$data[[i]]$text[1], start=-4)],
                                                    '<extra></extra>'
    ))
  }
  return(fig)
}

estima_tendencia <- function(serie) {
  print(serie)
  serie[is.na(serie)] <- 0
  # cálculo da série diária
  y <- c(1, diff(serie))
  
  # Cálculo das splines cúbicas
  spline_fit <- smooth.spline(y, df=40)
  poly <- SmoothSplineAsPiecePoly(spline_fit)
  # Maximos e minimos das splines
  zeros  <- solve(poly, deriv=1)
  minimos_ <- zeros[predict(poly, zeros, deriv=2)>0]
  minimos <- c()
  for (i in 1:(length(minimos_) - 1)) {
    if(minimos_[i+1] - minimos_[i] > 90) {
      minimos <- c(minimos, minimos_[i])
    }
  }
  
  N <- length(minimos)
  factors <- sapply(1:N, FUN=function(i){paste('I(d',i,'/(1+exp(b',i,'*(log(x)-e',i,'))))', sep='')})
  model_formula <- reformulate(termlabels = factors, response = 'y')
  t <- 1:length(serie)
  ip <- c()
  print(minimos)
  for (i in 1:N) {
    if(i == N) {
      yi <- serie[as.integer(minimos[i]):length(serie)] - serie[as.integer(minimos[i])-1]
      ti <- as.integer(minimos[i]):length(serie)
    } else {
      yi <- serie[as.integer(minimos[i]):as.integer(minimos[i+1]+1)]- serie[as.integer(minimos[i])-1]
      ti <- as.integer(minimos[i]):as.integer(minimos[i+1]+1)
    }
    modeloi <- drm(yi~ti, fct=LL2.3())
    ipi <- t(as.data.frame(modeloi$coefficients))
    colnames(ipi) <- paste(substr(colnames(ipi),1,1), i, sep='')
    ip <- cbind(ip, ipi)
  }
  dados <- data.frame(y=serie, x=t)
  fit <- nls(model_formula, dados, start=ip[1,])
  
  return(predict(fit))
}

#------------------------------------------------------------

# Defina o UI
ui <- dashboardPage(
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
                           sliderInput("date_slider", "Período", min = min(dados_estados$date), max = max(dados_estados$date),
                                       
                                       value = c(min(dados_estados$date), max(dados_estados$date)))
                       )
                )
                
              ),
              
              fluidRow(
                column(width = 12, 
                       h2(" 1) Confirmados")
                )
              ),
              
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
                column(width = 12, 
                       h2(" 2) Mortalidade")
                       )
                ),
                
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
                column(width = 12, 
                       h2(" 3) Doses de Vacinas administradas")
                )
              ),
              
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
                           sliderInput("data_slider1", "Período", min = min(dados_estados$date), max = max(dados_estados$date),
                                       
                                       value = c(min(dados_estados$date), max(dados_estados$date)))
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


#------------------------------------------------------------

# Defina o servidor
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
  
  
  output$grafico_series <- renderPlotly({
    est <- input$e_c
    cid <- input$cidade_filtro
    if(is.null(est) || est == '') {
      df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                  date <= input$date_slider[2])
    } else {
      if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
        df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                                date <= input$date_slider[2])
      }
      else {
        df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                             date <= input$date_slider[2])
      }
    }
    df_cidade$date <- as.Date(df_cidade$date)
    df_cidade <- corrige(df_cidade,"confirmed")
    titulo = paste("Casos confirmados em", cid, ', ', est)
    
    p <- grafico_series(df_cidade$date, df_cidade$confirmed, titulo, "Data", "Novos Confirmados Diários")
    
    return(p)
  })
  
  output$grafico_saz <- renderPlotly({
    est <- input$e_c
    cid <- input$cidade_filtro
    if(is.null(est) || est == '') {
      df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                  date <= input$date_slider[2])
    } else {
      if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
        df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                                 date <= input$date_slider[2])
      }
      else {
        df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                             date <= input$date_slider[2])
      }
    }
    df_cidade$date <- as.Date(df_cidade$date)
    df_cidade <- corrige(df_cidade,"confirmed")
    
    df_cidade <- df_cidade %>%
      slice(-1) %>%
      mutate(confirmed = diff(df_cidade$confirmed))
    
    p <- grafico_sazonal(df_cidade$date,df_cidade$confirmed,"Casos confirmados em anos sucessivos","Data","Novos confirmados","year")
    
    return(p)
  })
  
  
  output$grafico_series1 <- renderPlotly({
    est <- input$e_c
    cid <- input$cidade_filtro
    if(is.null(est) || est == '') {
      df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                  date <= input$date_slider[2])
    } else {
      if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
        df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                                 date <= input$date_slider[2])
      }
      else {
        df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                             date <= input$date_slider[2])
      }
    }
    df_cidade$date <- as.Date(df_cidade$date)
    df_cidade <- corrige(df_cidade,"deaths")
    titulo = paste("Numero de mortos em", cid, ', ', est)
    
    p <- grafico_series(df_cidade$date, df_cidade$deaths, titulo, "Data", "Novos Confirmados Diários")
    
    return(p)
  })
  
  
  output$grafico_saz1 <- renderPlotly({
    est <- input$e_c
    cid <- input$cidade_filtro
    if(is.null(est) || est == '') {
      df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                  date <= input$date_slider[2])
    } else {
      if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
        df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                                 date <= input$date_slider[2])
      }
      else {
        df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                             date <= input$date_slider[2])
      }
    }
    df_cidade$date <- as.Date(df_cidade$date)
    df_cidade <- corrige(df_cidade,"deaths")
    
    df_cidade <- df_cidade %>%
      slice(-1) %>%
      mutate(deaths = diff(df_cidade$deaths))
    
    p <- grafico_sazonal(df_cidade$date,df_cidade$deaths,"Número de mortos em anos sucessivos","Data","Mortes","year")
    
    return(p)
  })
  
  
  output$grafico_series2 <- renderPlotly({
    est <- input$e_c
    cid <- input$cidade_filtro
    if(is.null(est) || est == '') {
      df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                  date <= input$date_slider[2])
    } else {
      if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
        df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                                 date <= input$date_slider[2])
      }
      else {
        df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                             date <= input$date_slider[2])
      }
    }
    df_cidade$date <- as.Date(df_cidade$date)
    df_cidade <- corrige(df_cidade,"vaccines")
    titulo = paste("Doses de vacinas administradas por 10.000 habitantes em", cid, ', ', est)
    
    # print(df_cidade$vaccines)
    p <- grafico_series(df_cidade$date, df_cidade$vaccines/10000, titulo, "Data", "Novos Confirmados Diários")
    
    return(p)
  })
  
  output$grafico_saz2 <- renderPlotly({
    est <- input$e_c
    cid <- input$cidade_filtro
    if(is.null(est) || est == '') {
      df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
                                                                                  date <= input$date_slider[2])
    } else {
      if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
        df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
                                                                                 date <= input$date_slider[2])
      }
      else {
        df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
                                                                                             date <= input$date_slider[2])
      }
    }
    df_cidade$date <- as.Date(df_cidade$date)
    df_cidade <- corrige(df_cidade,"vaccines")
    # print(df_cidade$vaccines)
    # df_cidade <- df_cidade %>%
    #   mutate(vaccines = diff(df_cidade$vaccines))
    # print(df_cidade$vaccines)
    df_cidade <- df_cidade %>%
      slice(-1) %>%
      mutate(vaccines = diff(df_cidade$vaccines))
    
    p <- grafico_sazonal(df_cidade$date,df_cidade$vaccines/10000,"Doses de vacinas administradas por 10.000 habitantes em anos sucessivos","Data","Doses de vacinas","year")
    
    return(p)
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
    est1 <- input$e_c1
    cid1 <- input$cidade_filtro1
    est2 <- input$e_c2
    cid2 <- input$cidade_filtro2
    
    
    if((is.null(est1) && is.null(est2)) || (est1 == '' && est2 == '') || (is.null(est1) || is.null(est2)) || (est1 == '' || est2 == '')) {
      
      df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$data_slider1[1],
                                                                                  date <= input$data_slider1[2])
      
      p <- df_cidade %>%
        slice(-1) %>%
        mutate(vaccines = diff(df_cidade$vaccines)) %>%
        ggplot(aes(x = date, y = vaccines)) +
        geom_line(color = "blue") +
        labs(title = paste("Doses de vacinas administradas no Brasil"), x = "Data", y = "Novos Confirmados Diários") +
        theme_minimal() +
        scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y")
      
      fig <- ggplotly(p)
      return(fig)
      
    } else {
      if(!((is.null(est1) && is.null(est2)) || (est1 == '' && est2 == '')) && file.exists(paste('dados/dados', est1, cid1, '.csv')) && file.exists(paste('dados/dados', est2, cid2, '.csv'))) {
        df_cidade1 <- read.csv(paste('dados/dados', est1, cid1, '.csv')) %>% filter(date >= input$data_slider1[1],
                                                                                    date <= input$data_slider1[2])
        df_cidade1$date <- as.Date(df_cidade1$date)
        df_cidade1 <- corrige(df_cidade1,"vaccines")
        df_cidade1 <- df_cidade1 %>%
          slice(-1) %>%
          mutate(vaccines = diff(df_cidade1$vaccines))
        
        df_cidade2 <- read.csv(paste('dados/dados', est2, cid2, '.csv')) %>% filter(date >= input$data_slider1[1],
                                                                                    date <= input$data_slider1[2])
        df_cidade2$date <- as.Date(df_cidade2$date)
        df_cidade2 <- corrige(df_cidade2,"vaccines")
        df_cidade2 <- df_cidade2 %>%
          slice(-1) %>%
          mutate(vaccines = diff(df_cidade2$vaccines))
        
        df_1e2 <- rbind(df_cidade1,df_cidade2)
        
        p <- ggplot(df_1e2, aes(x = date, y = vaccines, color = administrative_area_level_3)) +
          geom_line() +
          labs(title = paste("Doses de vacinas administradas em", cid1,"e", cid2),
               x = "Ano",
               y = "Confirmados")
        
        fig <- ggplotly(p)
        return(fig)
      }
      else {
        df_cidade1 <- dados_estados %>% filter(administrative_area_level_2 == est1) %>% filter(date >= input$data_slider1[1],
                                                                                               date <= input$data_slider1[2])
        df_cidade1$date <- as.Date(df_cidade1$date)
        df_cidade1 <- corrige(df_cidade1,"vaccines")
        df_cidade1 <- df_cidade1 %>%
          slice(-1) %>%
          mutate(vaccines = diff(df_cidade1$vaccines))
        
        df_cidade2 <- dados_estados %>% filter(administrative_area_level_2 == est2) %>% filter(date >= input$data_slider1[1],
                                                                                               date <= input$data_slider1[2])
        df_cidade2$date <- as.Date(df_cidade2$date)
        df_cidade2 <- corrige(df_cidade2,"vaccines")
        df_cidade2 <- df_cidade2 %>%
          slice(-1) %>%
          mutate(vaccines = diff(df_cidade2$vaccines))
        
        df_1e2 <- rbind(df_cidade1,df_cidade2)
        
        p <- ggplot(df_1e2, aes(x = date, y = vaccines, color = administrative_area_level_2)) +
          geom_line() +
          labs(title = paste("Doses de vacinas administradas em", est1,"e", est2),
               x = "Ano",
               y = "Confirmados")
        
        fig <- ggplotly(p)
        return(fig)
      }
    }
  })
  
  #================================================================== END: DIFERENÇAS GEOGRÁFICAS
  
}

#------------------------------------------------------------

# Crie o aplicativo Shiny
shinyApp(ui, server)