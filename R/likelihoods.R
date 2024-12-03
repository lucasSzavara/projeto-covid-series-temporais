library(FAdist)
library(extraDistr)

dloglogistic <- function(t, theta) dllog(t, - 1 / theta$b, log(theta$e))

# Calcula a tendencia do modelo diário
model.trend <- function(d, theta, t, ddist = dloglogistic) {
  # 1 / b.sample[, i], log(e.sample[, i])
  p <- 0
  for (i in 1:length(d)) {
    p <- p + d[i] * ddist(t, theta[i, ])
  }
  return(p)
}

# Calcula a média do modelo
model.mean <- function(d, pars,
                       s,
                       phi,
                       theta,
                       t,
                       y,
                       g,
                       g.inv,
                       ddist = dloglogistic) {
  mu <- model.trend(d, pars, t, ddist = ddist) * s
  
  p <- length(phi)
  q <- length(theta)
  N <- length(y)
  Nn <- length(d)
  
  # AR: 
  eta_t <-  g(mu)
  
  for (i in 1:p) {
    y_t_j = y[1:(N - i)]
    eta_t[p:N] <-  eta_t[p:N] + phi[i] * (g(y_t_j) - mu[1:(N - i)])
  }
  
  
  # MA:
  epsilon <- 0
  
  for (i in 1:N) {
    epsilon[i] = g(y[i]) - eta_t[i]
    for (j in 1:min(i - 1, q)) {
      epsilon[i] <- epsilon[i] - theta[j] * epsilon[i - j];
    }
  }
  
  for (i in 1:N) {
    for (j in 1:min(i - 1, q)) {
      eta_t[i] <- eta_t[i] + theta[j] * epsilon[i - j];
    }
  }
  g.inv(eta_t)
}


binomial_loglik_loss <- function(theta, y, x, N) {
  # Nn <- length(theta) / 3
  # b <- theta[seq(1, Nn * 3, by = 3)]
  # d <- theta[seq(2, Nn * 3, by = 3)]
  # e <- theta[seq(3, Nn * 3, by = 3)]
  # 
  # p <- model.trend(d, data.frame(b = b, e = e), x[2:length(x)])
  # # p <- p + AR + MA
  # y <- diff(y)
  # p <- p / N
  # -sum(y*log(p) + (N - y) * log(1 - p))
  # 
  Nn <- length(theta) / 3
  b <- theta[seq(1, Nn * 3, by=3)]
  d <- theta[seq(2, Nn * 3, by=3)]
  e <- theta[seq(3, Nn * 3, by=3)]
  
  p <- 0
  for(i in 1:length(d)) {
    p <- p + d[i] / (1 + (x / e[i]) ^ b[i])
  }
  p <- diff(p)
  y <- diff(y)
  p <- p / N
  -sum(y*log(p) + (N - y) * log(1 - p))
}

bbinomial.neg.loglik.loss <- function(pars, y, x, N, Nn) {
  
  b <- pars[seq(1, Nn * 3, by = 3)]
  d <- pars[seq(2, Nn * 3, by = 3)]
  e <- pars[seq(3, Nn * 3, by = 3)]
  i <- Nn * 3
  
  y <- diff(y)

  
  # p <- model.trend(
  #   d = d,
  #   theta = data.frame(b = b, e = e),
  #   # s,
  #   # phi, theta,
  #   t = x[2:length(x)]
  #   # g,
  #   # g.inv
  # )
  p <- 0
  for (j in 1:length(d)) {
    p <- p + d[j] / (1 + (x / e[j]) ^ b[j])
  }
  p <- diff(p)
  # p <- p + AR + MA
  p <- p / N
  sigma <- pars[i + 1]
  alpha <-  p * sigma
  beta.param <-  sigma * (1 - p)
  -sum(dbbinom(y, N, alpha, beta.param, log = T))
}


garma.bbinomial.neg.loglik.loss <- function(pars, y, x, N, Nn, p, q) {
  
  b <- pars[seq(1, Nn * 3, by = 3)]
  d <- pars[seq(2, Nn * 3, by = 3)]
  e <- pars[seq(3, Nn * 3, by = 3)]
  i <- Nn*3
  s <- pars[(i + 1):(i + 7)]
  i <- i + 7
  phi <- pars[(i + 1):(i + p)]
  i <- i + p
  theta <- pars[(i + 1):(i + q)]
  i <- i + q
  y <- diff(y)
  y_ <- pmin(c(pmax(y, 0.5)), N - 0.5)
  
  g <- function(a) log(a / (N - a))
  g.inv <- function(g) N * exp(g) / (1 + exp(g))
  
  p <- model.mean(
    d = d,
    pars = data.frame(b = b, e = e),
    s,
    phi, theta,
    t,
    y_,
    g,
    g.inv
  )
  # p <- p + AR + MA
  p <- p / N
  sigma <- pars[i + 1]
  alpha <-  p * sigma
  beta <-  sigma * (1 - p)
  -sum(log(beta(y + alpha, N - y + beta)) - log(beta(alpha, beta)))
}
# 
# library(microbenchmark)
# mu <- c(1, 2, 1, 0.5, 1) / 3
# sigma <- 3
# N <- 3
# y <- rBB(10, mu = mu, sigma = sigma, bd = 3)
# alpha = mu * sigma;
# beta = sigma * (1 - mu);
# sum(log(choose(N, y)) + log(beta(y + alpha, 3 - y + beta)) - log(beta(alpha, beta)))
# 
# sum(dBB(y, mu = mu,
#         sigma = sigma, log = T, bd = N))
# m_out <- microbenchmark(sum(dBB(y, mu = mu,
#                                  sigma = sigma, log = T, bd = N)),
#                         {
#                           alpha = mu * sigma;
#                           beta = sigma * (1 - mu);
#                           sum(log(beta(y + alpha, 3 - y + beta)) - log(beta(alpha, beta)))
#                           },
#                         times = 1000
# )
# m_out

