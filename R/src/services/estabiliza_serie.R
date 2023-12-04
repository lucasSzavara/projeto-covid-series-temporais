remove_tendencia <- function(serie) {
  # Recebe a série acumulada e retorna a série sem tendência e não diferenciada.
  return(serie - estima_tendencia(serie))
}

padroniza_variancia <- function(serie, width=180) {
  # Recebe a série acumulada sem tendencia e retorna a série com variancia padronizada
  serie_diaria <- c(0, diff(serie))
  ysd <- rollapply(serie_diaria, width=width, FUN = sd, fill = 1)
  index <- which(!is.na(ysd))
  y <- serie_diaria[index]/ysd[index]
  return(y)
}

remove_sazonalidade <- function(serie) {
  # Recebe a série diaria sem tendencia e variancia padronizada e retorna a série sem sazonalidade, diaria.
  return(c(rep(0, 7), diff(serie, lag=7)))
}


estabiliza_serie <- function(serie, width=180) {
  # Recebe uma série com tendência, e a retorna estabilizada
  return(remove_sazonalidade(padroniza_variancia(remove_tendencia(serie), width=width)))
}