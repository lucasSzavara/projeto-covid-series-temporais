# # Carregar pacote
# library('COVID19')
library(FAdist)
library(MASS)
library(extraDistr)


gera.dados.binom <- function(d.true, e.true, b.true, s.true, theta, phi, n, pop) {
  g <- function(a) log(a / (1 - a))
  g.inv <- function(g) exp(g) / (1 + exp(g))
  n_ondas <- length(d.true)
  mu <- rep(0, n)
  
  p <- length(phi)
  q <- length(theta)
  
  for (k in 1:n_ondas) {
    trend <- d.true[k] * dllog(1:n, 1 / b.true[k], log(e.true[k]))
    mu <- mu + s.true * trend
  }
  
  
  y <- c()
  y[1] <- rbinom(1, pop, mu[1] / pop)
  eta <- c(g(mu[1] / pop))
  y_ <- min(max(y[1], 0.5), pop - 0.5)
  epsilon <- c(g(y_ / pop) - eta[1])
  
  for (i in 2:n) {
    # i <- 2
    eta[i] <- g(mu[i] / pop)
    
    if (q > 0) for (j in 1:min(q, i - 1)) {
      eta[i] <- eta[i] + theta[j] * epsilon[i - j]
    }
    
    # if (i >= 8) {
    #   for (j in 1:min(Q, i - 1)) {
    #     # j <- 1
    #     y_ <- min(max(y[i - 7*j], 0.5), 4e7 - 0.5)
    #     eta[i] <- eta[i] + theta.s[j] * (g(y_/ 4e7) - eta[i - 7*j])
    #   }
    # }
    
    y[i] <- rbinom(1, pop, g.inv(eta[i]))
    y_ <- min(max(y[i], 0.5), pop - 0.5)
    epsilon[i] <- (g(y_ / pop) - eta[i])
  }
  list(y=y, mu=mu)
}


gera.dados.pois <- function(d.true, e.true, b.true, s.true, theta, phi, n, pop) {
  g <- function(a) log(a)
  g.inv <- function(g) exp(g)
  n_ondas <- length(d.true)
  mu <- rep(0, n)
  
  p <- length(phi)
  q <- length(theta)
  
  for (k in 1:n_ondas) {
    trend <- d.true[k] * dllog(1:n, 1 / b.true[k], log(e.true[k]))
    mu <- mu + s.true * trend
  }
  
  
  y <- c()
  y[1] <- rpois(1, mu[1])
  eta <- c(g(mu[1]))
  y_ <- max(y[1], 0.5)
  epsilon <- c(g(y_) - eta[1])
  
  for (i in 2:n) {
    # i <- 2
    eta[i] <- g(mu[i])
    
    if (q > 0) for (j in 1:min(q, i - 1)) {
      eta[i] <- eta[i] + theta[j] * epsilon[i - j]
    }
    
    # if (i >= 8) {
    #   for (j in 1:min(Q, i - 1)) {
    #     # j <- 1
    #     y_ <- min(max(y[i - 7*j], 0.5), 4e7 - 0.5)
    #     eta[i] <- eta[i] + theta.s[j] * (g(y_/ 4e7) - eta[i - 7*j])
    #   }
    # }
    
    y[i] <- rpois(1, g.inv(eta[i]))
    y_ <- max(y[i], 0.5)
    epsilon[i] <- (g(y_) - eta[i])
  }
  list(y=y, mu=mu)
}


gera.dados.negbin <- function(d.true, e.true, b.true, s.true, theta, phi, n, pop, prec) {
  g <- function(a) log(a)
  g.inv <- function(g) exp(g)
  n_ondas <- length(d.true)
  mu <- rep(0, n)
  
  p <- length(phi)
  q <- length(theta)
  
  for (k in 1:n_ondas) {
    trend <- d.true[k] * dllog(1:n, 1 / b.true[k], log(e.true[k]))
    mu <- mu + s.true * trend
  }
  
  
  y <- c()
  y[1] <- rnegbin(1, mu[1], prec)
  eta <- c(g(mu[1]))
  y_ <- max(y[1], 0.5)
  epsilon <- c(g(y_) - eta[1])
  
  for (i in 2:n) {
    # i <- 2
    eta[i] <- g(mu[i])
    
    if (q > 0) for (j in 1:min(q, i - 1)) {
      eta[i] <- eta[i] + theta[j] * epsilon[i - j]
    }
    
    # if (i >= 8) {
    #   for (j in 1:min(Q, i - 1)) {
    #     # j <- 1
    #     y_ <- min(max(y[i - 7*j], 0.5), 4e7 - 0.5)
    #     eta[i] <- eta[i] + theta.s[j] * (g(y_/ 4e7) - eta[i - 7*j])
    #   }
    # }
    
    y[i] <- rnegbin(1, g.inv(eta[i]), prec)
    y_ <- max(y[i], 0.5)
    epsilon[i] <- (g(y_) - eta[i])
  }
  list(y=y, mu=mu, eta=eta, epsilon=epsilon)
}

gera.dados.betabinom <- function(d.true, e.true, b.true, s.true, theta, phi, n, pop, sigma) {
  g <- function(a) log(a / (1 - a))
  g.inv <- function(g) exp(g) / (1 + exp(g))
  n_ondas <- length(d.true)
  mu <- rep(0, n)
  
  p <- length(phi)
  q <- length(theta)
  
  for (k in 1:n_ondas) {
    trend <- d.true[k] * dllog(1:n, 1 / b.true[k], log(e.true[k]))
    mu <- mu + s.true * trend
  }
  
  
  y <- c()
  alpha <- c()
  beta <- c()
  p_ <- c()
  p_[1] <- mu[1] / pop
  alpha[1] <-  p_[1] * sigma
  beta[1] <-  sigma * (1 - p_[1])
  y[1] <- rbbinom(1, pop, alpha[1], beta[1])
  eta <- c(g(p_[1]))
  y_ <- min(max(y[1], 0.5), pop - 0.5)
  epsilon <- c(g(y_ / pop) - eta[1])
  
  for (i in 2:n) {
    # i <- 2
    eta[i] <- g(mu[i] / pop)
    if (q > 0) {
      for (j in 1:min(q, i - 1)) {
        eta[i] <- eta[i] + theta[j] * epsilon[i - j]
      }
    }
    
    # if (i >= 8) {
    #   for (j in 1:min(Q, i - 1)) {
    #     # j <- 1
    #     y_ <- min(max(y[i - 7*j], 0.5), 4e7 - 0.5)
    #     eta[i] <- eta[i] + theta.s[j] * (g(y_/ 4e7) - eta[i - 7*j])
    #   }
    # }
    p_[i] <- g.inv(eta[i])
    alpha[i] <-  p_[i] * sigma
    beta[i] <-  sigma * (1 - p_[i])
    y[i] <- rbbinom(1, pop, alpha[i], beta[i])
    y_ <- min(max(y[i], 0.5), pop - 0.5)
    epsilon[i] <- (g(y_ / pop) - eta[i])
  }
  list(y=y, mu=mu, eta=eta, alpha=alpha, beta=beta, p=p_, epsilon=epsilon)
}


# 
# #------------------------------------------------------------
# 
# # Salvar dados de cidades do Brasil em arquivos separados
# x <- covid19(country=c('Brazil'), level=3, verbose=F, vintage = "2023-09-30")
# cidades <- c()
# estados <- c()
# for(uf in unique(x$administrative_area_level_2)){
#   cat('\nComeçando estado: ', uf)
#   for(cidade in unique(x[x$administrative_area_level_2 == uf,]$administrative_area_level_3)) {
#     cat('\nComeçando cidade: ', cidade)
#     cidades <- c(cidades, cidade)
#     estados <- c(estados, uf)
#     write.csv(x[x$administrative_area_level_2 == uf & x$administrative_area_level_3 == cidade,], paste('./dados/cidades/dados', uf, cidade,'.csv'), row.names = F)
#   }
# }
# 
# # Salvar o nome de todos estados e cidades
# aux <- data.frame(estados, cidades)
# write.csv(aux[order(aux$estados, aux$cidades), ], './dados/estados_cidades.csv', row.names = F)
# 
# #------------------------------------------------------------
# 
# # Salvar dados de estados do Brasil em arquivos separados
# x <- covid19(country=c('Brazil'), level=2, verbose=F, vintage = "2023-09-30")
# for(uf in unique(x$administrative_area_level_2)){
#   cat('\nComeçando estado: ', uf)
#   write.csv(x[x$administrative_area_level_2 == uf,], paste('./dados/estados/dados', uf,'.csv'), row.names = F)
#   }
# 
# #------------------------------------------------------------
# 
# #  Salvar dados do Brasil
# z <- covid19(country=c('Brazil'), level=1, verbose=F, vintage = "2023-09-30")
# write.csv(z, './dados/pais/dados_pais.csv', row.names = F)
