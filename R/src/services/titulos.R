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


titulo_saz <- function(variavel, est, cid) {
  titulo <- ""
  
  if (variavel == "confirmed") {
    aux <- paste("Casos confirmados em anos sucessivos")
  } else {
    if (variavel == "deaths") {
      aux <- paste("Número de mortos em anos sucessivos")
    } else {
      aux <- paste("Doses administradas/10000 em anos sucessivos")
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




