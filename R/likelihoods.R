binomial_loglik_loss <- function(theta, y, x, N) {
  
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