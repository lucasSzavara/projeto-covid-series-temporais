calcula_q <- function(dados) {
  # Estima a ordem de um processo de médias móveis a partir da série estacionaria
  acf_values <- ACF(dados, lag_max=90)$acf
  intercept_ind <- which(abs(acf_values) < qnorm((1 + 0.96) / 2) / sqrt(length(dados$y)))
  comp <- which(c(diff(intercept_ind), 2) != 1)
  inds <- c(comp[1],abs(rev(diff(rev(comp)))))
  ind <- which(c(comp[1],abs(rev(diff(rev(comp))))) > 5)[1]
  soma <- sum(inds[1:ind-1])
  valor <- comp[ind]
  complemento <- valor - sum(1:valor %in% intercept_ind)
  soma <- soma + complemento
  return(soma)
}

calcula_p <- function(dados) {
  pacf_values <- PACF(dados, lag_max=90)$pacf
  intercept_ind <- which(abs(pacf_values) < qnorm((1 + 0.96) / 2) / sqrt(length(dados$y)))
  comp <- which(c(diff(intercept_ind),2) !=1)
  inds <- c(comp[1],abs(rev(diff(rev(comp)))))
  ind <- which(c(comp[1],abs(rev(diff(rev(comp))))) > 5)[1]
  soma <- sum(inds[1:ind-1])
  valor <- comp[ind]
  complemento <- valor - sum(1:valor %in% intercept_ind)
  soma <- soma + complemento
  return(soma)
}