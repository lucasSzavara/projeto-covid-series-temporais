# Funções que transformam e filtram/selecionam os dados

#-----------------------------------------------------------------------------------------------

titulo_series <- function(variavel, est, cid) {
  titulo <- ""
  
  if (variavel == "confirmed") {
    aux <- paste("Casos confirmados")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Número de mortos")
    } else {
      aux <- paste("Doses administradas/10000")
    }
  }
  
  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid)
    } else {
      titulo <- paste(aux, "em", est)
    }
  }
  
  return(titulo)
}

#-----------------------------------------------------------------------------------------------

titulo_series_tendencia <- function(variavel, est, cid) {
  titulo <- ""
  
  if (variavel == "confirmed") {
    aux <- paste("Casos confirmados")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Número de mortos")
    } else {
      aux <- paste("Doses administradas/10000")
    }
  }
  
  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil e sua tendência")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid, "e sua tendência")
    } else {
      titulo <- paste(aux, "em", est, "e sua tendência")
    }
  }
  
  return(titulo)
}

#-----------------------------------------------------------------------------------------------

titulo_saz <- function(variavel, est, cid, periodo) {
  titulo <- ""
  
  if (periodo == "year") {
    if (variavel == "confirmed") {
      aux <- paste("Casos confirmados em anos sucessivos")
    } else {
      if (variavel == "deaths") {
        aux <- paste("Número de mortos em anos sucessivos")
      } else {
        aux <- paste("Doses administradas/10000 em anos sucessivos")
      }
    }
  }
  
  if (periodo == "week") {
    if (variavel == "confirmed") {
      aux <- paste("Casos confirmados em diferentes semanas")
    } else {
      if (variavel == "deaths") {
        aux <- paste("Número de mortos em diferentes semanas")
      } else {
        aux <- paste("Doses administradas/10000 em diferentes semanas")
      }
    }
  }

  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid)
    } else {
      titulo <- paste(aux, "em", est)
    }
  }
  
  return(titulo)
}

#-----------------------------------------------------------------------------------------------

titulo_series_res <- function(variavel, est, cid) {
  titulo <- ""
  
  if (variavel == "confirmed") {
    aux <- paste("Residuo da tendência padronizado para casos confirmados")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Residuo da tendência padronizado para número de mortos")
    } else {
      aux <- paste("Residuo da tendência padronizado para doses administradas/10000")
    }
  }
  
  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil e sua sazonalidade")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid, "e sua sazonalidade")
    } else {
      titulo <- paste(aux, "em", est, "e sua sazonalidade")
    }
  }
  
  return(titulo)
}

#-----------------------------------------------------------------------------------------------

titulo_series_esta <- function(variavel, est, cid) {
  titulo <- ""
  
  if (variavel == "confirmed") {
    aux <- paste("Gráfico estacionario de casos confirmados")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Gráfico estacionario do número de mortos")
    } else {
      aux <- paste("Gráfico estacionario de doses administradas/10000")
    }
  }
  
  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid)
    } else {
      titulo <- paste(aux, "em", est)
    }
  }
  
  return(titulo)
}

#-----------------------------------------------------------------------------------------------

titulo_series_acf <- function(variavel, est, cid) {
  titulo <- ""
  
  if (variavel == "confirmed") {
    aux <- paste("Gráfico da autocorrelação de Casos confirmados")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Gráfico da autocorrelação do Número de mortos")
    } else {
      aux <- paste("Gráfico da autocorrelação de Doses administradas/10000")
    }
  }
  
  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid)
    } else {
      titulo <- paste(aux, "em", est)
    }
  }
  
  return(titulo)
}

titulo_series_pacf <- function(variavel, est, cid) {
  titulo <- ""
  
  if (variavel == "confirmed") {
    aux <- paste("Gráfico da autocorrelação parcial de Casos confirmados")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Gráfico da autocorrelação parcial do Número de mortos")
    } else {
      aux <- paste("Gráfico da autocorrelação parcial de Doses administradas/10000")
    }
  }
  
  if (is.null(est) || est == '') {
    titulo <- paste(aux, "no Brasil")
  } else {
    if (!(is.null(cid) || cid == '')) {
      titulo <- paste(aux, "em", est, "-", cid)
    } else {
      titulo <- paste(aux, "em", est)
    }
  }
  
  return(titulo)
}