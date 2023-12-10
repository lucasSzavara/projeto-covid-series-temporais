# Funções que transformam e filtram/selecionam os dados
library(dplyr)
#-----------------------------------------------------------------------------------------------

# Carregar Dados Salvos(até 2023-09-30)
dados_pais <- read.csv('dados/pais/dados_pais.csv')

#-----------------------------------------------------------------------------------------------

# Transformar dados
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
carregar_dados <- function(est, cid, date_slider, variavel, pad_ = FALSE) {
  
  if (is.null(est) || est == '') {
    df <- dados_pais %>%
      filter(date >= date_slider[1], date <= date_slider[2])
  } else {
    
    file_path <- paste('dados/cidades/dados', est, cid, '.csv')
    if (!(is.null(cid) || cid == '') && file.exists(file_path)) {
      df <- read.csv(file_path) %>%
        filter(date >= date_slider[1], date <= date_slider[2])
      
    } else {
      file_path <- paste('dados/estados/dados', est, '.csv')
      df <- read.csv(file_path) %>%
        filter(administrative_area_level_2 == est) %>%
        filter(date >= date_slider[1], date <= date_slider[2])
    }
  }
  df$date <- as.Date(df$date)
  
  if (pad_) {
    df <- pad(df)
  }
  
  df <- corrige(df, variavel)
  return(df)
}