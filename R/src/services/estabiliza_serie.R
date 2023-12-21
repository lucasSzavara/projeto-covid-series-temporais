remove_tendencia <- function(serie, invertible=F) {
  # Recebe a série acumulada e retorna a série sem tendência e não diferenciada.
  if (invertible) {
    tend <- estima_tendencia(serie, params=T)
    return(list(
      serie=serie - tend$tendencia,
      params=tend$params
      ))
  }
  return(serie - estima_tendencia(serie))
}

padroniza_variancia <- function(serie, width=90, invertible=F) {
  # Recebe a série acumulada sem tendencia e retorna a série com variancia padronizada
  serie_diaria <- c(0, diff(serie))
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

remove_sazonalidade <- function(serie) {
  # Recebe a série diaria sem tendencia e variancia padronizada e retorna a série sem sazonalidade, diaria.
  return(c(rep(0, 7), diff(serie, lag=7)))
}


estabiliza_serie <- function(serie, width=90, invertible=F) {
  # Recebe uma série com tendência, e a retorna estabilizada
  if(invertible) {
    tend <- serie %>% remove_tendencia(invertible=invertible)
    var  <- tend$serie %>% padroniza_variancia(width=width, invertible=invertible)
    serie <- var$serie %>% remove_sazonalidade
    return(list(serie=serie, params=tend$params, sd=var$sd))
  }
  return(serie %>% remove_tendencia() %>%
           padroniza_variancia(width=width) %>%
           remove_sazonalidade()
         )
}