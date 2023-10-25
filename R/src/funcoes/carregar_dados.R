# Funções que transformam e filtram/selecionam os dados

#-----------------------------------------------------------------------------------------------

# Carregar Dados Salvos(até 2023-09-30)
dados_pais <- read.csv('dados_pais.csv')
# dados_estados <- covid19(country = c('Brazil'), level=2, verbose=F)
dados_estados <- covid19(country = c('Brazil'), level=1, verbose=F) #rodar testes mais rapido

#-----------------------------------------------------------------------------------------------

# Transformar dados acumulados em diários
corrige <- function(df, variavel){
  df[is.na(df)] <- 0
  for(i in 2:length(df[[variavel]])){
    if(df[i,variavel] < df[i-1,variavel]){
      df[i,variavel] <- df[i-1,variavel]
    }
  }
  return(df)
}

#-----------------------------------------------------------------------------------------------

# Carregar dados especificados(cidade, estado ou pais)
carregar_dados <- function(est, cid, date_slider, variavel) {
  # loc <- NULL
  
  if (is.null(est) || est == '') {
    # loc <- c('Brazil')
    df <- dados_pais %>%
      filter(date >= date_slider[1], date <= date_slider[2])
  } else {
    
    file_path <- paste('dados/dados', est, cid, '.csv')
    if (!(is.null(cid) || cid == '') && file.exists(file_path)) {
      # loc <- c(est, cid)
      df <- read.csv(file_path) %>%
        filter(date >= date_slider[1], date <= date_slider[2])
      
    } else {
      # loc <- c(est)
      df <- dados_estados %>%
        filter(administrative_area_level_2 == est) %>%
        filter(date >= date_slider[1], date <= date_slider[2])
    }
  }
  df$date <- as.Date(df$date)
  df <- corrige(df, variavel)
  return(df)
}