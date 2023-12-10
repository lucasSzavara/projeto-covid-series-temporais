calcula_q <- function(dados) {
  # Estima a ordem de um processo de médias móveis a partir da série estacionaria
  acf_values <- ACF(dados, lag_max=90)$acf
  intercept_ind <- which(abs(acf_values) < qnorm((1 + 0.96) / 2) / sqrt(length(acf_values)))
  q <- NULL
  for (i in intercept_ind) {
    zeros <- 0
    for (j in i:(i+6)) {
      if (sum(j == intercept_ind) == 0) {
        break
      }
      zeros <- zeros + 1
    }
    if (zeros == 7) {
      q <- i
      break
    }
  }
  return(q)
}

calcula_p <- function(dados) {
  pacf_values <- PACF(dados, lag_max=90)$pacf
  intercept_ind <- which(abs(acf_values) < qnorm((1 + 0.96) / 2) / sqrt(length(pacf_values)))
  p <- NULL
  for (i in intercept_ind) {
    zeros <- 0
    for (j in i:(i+6)) {
      if (sum(j == intercept_ind) == 0) {
        break
      }
      zeros <- zeros + 1
    }
    if (zeros == 7) {
      p <- i
      break
    }
  }
  return(p)
}


# acf_values <- c(15, 5, 5, 5, 5, 5, -6, -6, 0, 2, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# intercept_ind <- which(abs(acf_values) < qnorm((1 + 0.96) / 2) / sqrt(length(acf_values)))
# q <- NULL
# for (i in intercept_ind) {
#   zeros <- 0
#   for (j in i:(i+6)) {
#     if (sum(j == intercept_ind) == 0) {
#       break
#     }
#     zeros <- zeros + 1
#   }
#   if (zeros == 7) {
#     q <- i
#     break
#   }
# }
