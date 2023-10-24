# grafico_series <- function(datas, series, titulo_grafico, eixo_x, eixo_y) {
#   tendencias <- estima_tendencia(series)
#   p <- as.data.frame(cbind(serie=series)) %>%
#     slice(-1) %>%
#     mutate(serie_mutada = diff(series)) %>%
#     ggplot(aes(x = datas[2:length(datas)], y = serie_mutada)) +
#     geom_line(color = "blue") +
#     labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
#     theme_minimal() +
#     scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y") +
#     geom_line(aes(x=datas[2:length(datas)], y=diff(tendencias)), color='red')
# 
#   fig <- ggplotly(p)
#   return(fig)
# 
# }
# 
# # Casos confirmados
# grafico_series_casos <- function(input,output){
#   est <- input$e_c
#   cid <- input$cidade_filtro
#   if(is.null(est) || est == '') {
#     df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
#                                                                                 date <= input$date_slider[2])
#   } else {
#     if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
#       df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
#                                                                                date <= input$date_slider[2])
#     }
#     else {
#       df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
#                                                                                            date <= input$date_slider[2])
#     }
#   }
#   df_cidade$date <- as.Date(df_cidade$date)
#   df_cidade <- corrige(df_cidade,"confirmed")
#   titulo = paste("Casos confirmados em", cid, ', ', est)
#   
#   p <- grafico_series(df_cidade$date, df_cidade$confirmed, titulo, "Data", "Novos Confirmados Diários")
#   
#   return(p)
# }
# 
# #-----------------------------------------------------------------------------------------------
# 
# # Mortes
# 
# grafico_series_mortes <- function(input,output){
#   est <- input$e_c
#   cid <- input$cidade_filtro
#   if(is.null(est) || est == '') {
#     df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
#                                                                                 date <= input$date_slider[2])
#   } else {
#     if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
#       df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
#                                                                                date <= input$date_slider[2])
#     }
#     else {
#       df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
#                                                                                            date <= input$date_slider[2])
#     }
#   }
#   df_cidade$date <- as.Date(df_cidade$date)
#   df_cidade <- corrige(df_cidade,"deaths")
#   titulo = paste("Numero de mortos em", cid, ', ', est)
#   
#   p <- grafico_series(df_cidade$date, df_cidade$deaths, titulo, "Data", "Novos Confirmados Diários")
#   
#   return(p)
# }
# 
# #-----------------------------------------------------------------------------------------------
# 
# # Vacinas
# 
# grafico_series_vacinas <- function(input,output){
#   est <- input$e_c
#   cid <- input$cidade_filtro
#   if(is.null(est) || est == '') {
#     df_cidade <- covid19(country = c('Brazil'), level=1, verbose=F)  %>% filter(date >= input$date_slider[1],
#                                                                                 date <= input$date_slider[2])
#   } else {
#     if(!(is.null(cid) || cid == '') && file.exists(paste('dados/dados', est, cid, '.csv'))) {
#       df_cidade <- read.csv(paste('dados/dados', est, cid, '.csv')) %>% filter(date >= input$date_slider[1],
#                                                                                date <= input$date_slider[2])
#     }
#     else {
#       df_cidade <- dados_estados %>% filter(administrative_area_level_2 == est) %>% filter(date >= input$date_slider[1],
#                                                                                            date <= input$date_slider[2])
#     }
#   }
#   df_cidade$date <- as.Date(df_cidade$date)
#   df_cidade <- corrige(df_cidade,"vaccines")
#   titulo = paste("Doses de vacinas administradas por 10.000 habitantes em", cid, ', ', est)
#   
#   # print(df_cidade$vaccines)
#   p <- grafico_series(df_cidade$date, df_cidade$vaccines/10000, titulo, "Data", "Novos Confirmados Diários")
#   
#   return(p)
# }

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

# Funçao geral para o grafico de series

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

render_grafico_series <- function(input, variavel, escala=1) {
  est <- input$e_c
  cid <- input$cidade_filtro
  slider <- input$date_slider

  df <- carregar_dados(est, cid, slider, variavel)

  # Verifica se a variável 'variavel' é uma coluna válida nos dados
  if (!(variavel %in% names(df))) {
    stop("A variável 'variavel' não é uma coluna válida nos dados.")
  }
  
  titulo <- titulo_series(variavel, est, cid)

  p <- grafico_series(df$date, df[[variavel]] / escala, titulo, "Data", "Novos Confirmados Diários")

  return(p)
}