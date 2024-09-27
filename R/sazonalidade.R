library(fpp3)
library(dplyr)



sazonalidade_mp <- function(y, t) {
  saz <- c()
  for (i in 1:7){
    ind <- c(rep(F, i-1), T, rep(F, 7-i))
    dias <- (y / t)[ind]
    saz_i <- weighted.mean(dias, t[ind])
    saz <- c(saz, saz_i)
  }
  saz
}


padroniza_variancia <- function(serie, width=90, invertible=F) {
  # Recebe a série acumulada sem tendencia e retorna a série com variancia padronizada
  serie_diaria <- c(0, diff(serie))
  if (width == 1) {
    if (invertible) {
      return(list(
        serie=serie,
        sd=rep(1, length(serie))
      ))
    }
    return(serie)
  }
  ysd <- rollapply(serie_diaria, width=width, FUN = sd, fill = NA)
  index <- which(!is.na(ysd))
  y <- serie_diaria[index]/ysd[index]
  if (invertible) {
    return(list(
      serie=y,
      sd=ysd[index]
    ))
  }
  return(y)
}


estima_sazonalidade <- function(serie, datas) {
  # Pega uma série sem tendência, e modela sua sazonalidade. Para os dados
  # de covid, temos um melhor resultado ao ajustar a série diferenciada dos dados
  # menos a tendencia ajustada (diff(serie-tendencia))
  # usar variaveis dummy
  df <- data.frame(serie, data=datas)
  modelo_sazonalidade <- model(
    df %>% as_tsibble(),
    TSLM(serie ~ season(K = 3))
  )
  return(modelo_sazonalidade)
}

