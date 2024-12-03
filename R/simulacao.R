source('./tendencia.R')
source('./sazonalidade.R')
library(ggplot2)
library(ggpubr)
library(SimDesign)
library(stats)
library(dplyr)

# e <- 500
# b <- -35
# d <- 10
# t <- 1:1000
# acumulada <- expression(d / (1+exp(b*(log(t)-log(e)))))
# plot(acumulada)
# diaria <- D(acumulada, 't')

# segunda_derivada <- D(diaria, 't')
# terceira_derivada <- D(segunda_derivada, 't')
f <- as.function(c(alist(t=), list(diaria)))
plot(f(t))
# # uniroot(f=f, interval=c(1, 50))
# x<-1:100000/100
# pontos <- f(x)
# plot(y=pontos, x=x)
# abline(v=500)
# abline(h=0)
# sort(abs(pontos)[100:100000])
# zeros <- which(abs(pontos)[100:100000] < 1e-8)
# zeros <- unique(round(zeros/100))
# abline(v=zeros)

Design_1 <- createDesign(e=c(50, 100, 500),
                         b=c(-2, -10, -30, -50, -100),
                         d=c(10, 20, 30),
                         t_max=c(50, 100, 200, 300, 400, 500, 530, 600, 800),
                         N=c(3000, 5000, 15000),
                         method=c('kriston', 'splines'),
                         param=c(1:3 * 4, 2:5 * 10),
                         subset = !(e == 500 & t_max < 300) &
                                  !(e < 500 & t_max > 200) &
                                  !(method == 'kriston' & param %% 10 == 0) &
                                  !(method == 'splines' & param %% 10 != 0) &
                                  !(param %% 10 != 0  & param > t_max))

Generate <- function(condition, fixed_objects=FALSE) {
  e <- condition$e
  b <- condition$b
  d <- condition$d
  t <- 1:condition$t_max
  N <- condition$N
  n <- condition$t_max - 1
  media <- d / (1+exp(b*(log(t)-log(e))))
  p <- diff(media) / N
  cumsum(c(0, rbinom(n, rep(N, n), p)))
}


Analyse <- function(condition, dat, fixed_objects=FALSE) {
  resultado <- estima_tendencia(as.vector(dat),
                                params=T, 
                                param=condition$param, 
                                wcmethod=condition$method, dates = 1:length(as.vector(dat)))
  fit_tend <- resultado$tendencia
  params_tend <- resultado$params
  c(length(params_tend)/3, fit_tend)
}

Summarise <- function(condition, results, fixed_objects=FALSE) {
  n_medio <- mean(results[, 1])
  vies_n <- bias(results[, 1], parameter=1)
  RMSE_n <- RMSE(results[, 1], parameter=1)
  ajustes <- results[, -1]
  e <- condition$e
  b <- condition$b
  d <- condition$d
  t <- 1:condition$t_max
  media <- d / (1+exp(b*(log(t)-log(e))))
  eqm <-  mean(apply(ajustes, 2, FUN = function(r) r - media)^2)
  smape <- mean(abs(apply(ajustes, 2, FUN = function(r) r - media))
                /((apply(ajustes, 2, FUN = function(r) r + media))/2)
  )
  c(n_medio=n_medio,
    vies_n=vies_n,
    RMSE_n=RMSE_n,
    eqm=eqm,
    smape=smape
    )
}

res <- runSimulation(Design_1, replications = 5, generate=Generate, 
                     analyse=Analyse, summarise=Summarise, parallel = T)


write.csv(res, './simulacao.csv')


source('./tendencia.R')
library("ggplot2")
library(dplyr)
library("parallel")
library(tidyr)
library(data.table)
library(FAdist)

options(mc.cores = 4)

n_ondas <- 2
mu <- rep(0, 1200)
d.sd <- c(5e5, 1e6)
e.true <- c(160, 700)
b.true <- c(3, 10)
s.original <- c(1, 1, 1, 1.1, 1.03, 0.2, 0.25)
s.true <- s.original / mean(s.original)
d.true <- d.sd # exp(mean(log(s.true)))
p <- 0
q <- 1
Q <- 1
theta <- c(-0.8)
theta.s <- c(-0.7)
phi <- c()
mu.ws <- rep(0, 1200)
for (k in 1:n_ondas) {
  trend <- d.sd[k] * dllog(1:1200, 1 / b.true[k], log(e.true[k]))
  mu <- mu + s.true * trend
  mu.ws <- mu.ws + trend
}
plot(1:1200, mu)

g <- function(a) log(a / (1 - a))
g.inv <- function(g) exp(g) / (1 + exp(g))

y <- c()
y[1] <- rbinom(1, 4e7, mu[1] / 4e7)
eta <- c(g(mu[1] / 4e7))

for (i in 2:1200) {
  # i <- 2
  eta[i] <- g(mu[i] / 4e7)
  for (j in 1:min(q, i - 1)) {
    # j <- 1
    y_ <- min(max(y[i - j], 0.5), 4e7 - 0.5)
    eta[i] <- eta[i] + theta[j] * (g(y_/ 4e7) - eta[i - j])
  }
  
  if (i >= 8) {
    for (j in 1:min(Q, i - 1)) {
      # j <- 1
      y_ <- min(max(y[i - 7*j], 0.5), 4e7 - 0.5)
      eta[i] <- eta[i] + theta.s[j] * (g(y_/ 4e7) - eta[i - 7*j])
    }
  }
  
  y[i] <- rbinom(1, 4e7, g.inv(eta[i]))
}
plot(1:1200, y)

fit <- estima_tendencia(cumsum(y), 4e7, params = T, param = 25)
fit$tendencia %>% plot()

x <- data.frame(real = y,
                ajuste = c(1, diff(fit$tendencia)),
                t=1:length(y),
                mu=mu,
                mu.ws=mu.ws
                )
x %>% 
  ggplot(aes(x=t)) +
  geom_point(aes(y = real), alpha=0.3) +
  geom_line(aes(y = mu, colour = 'Mu')) +
  geom_line(aes(y = mu.ws, colour = 'Tendência')) +
  geom_line(aes(y = ajuste, colour = 'Ajuste'))

d.fit <- c(fit$params['d1'], fit$params['d2'])

source('./sazonalidade.R')
sazonalidade <- sazonalidade_mp(y, x$ajuste)
sazonalidade / mean(sazonalidade) # exp(mean(log(sazonalidade)))
s.true
d.fit
d.true
sum(y)
sum(d.true)
sum(d.fit)

x$y_hat <- x$ajuste * sazonalidade
x %>% 
  ggplot(aes(x=t)) +
  geom_point(aes(y = real), alpha = 0.3) +
  geom_line(aes(y = y_hat, colour = 'Ajuste sazonal')) +
  geom_line(aes(y = ajuste, colour = 'Ajuste'))

x$residuo <- (x$real - x$y_hat) / sqrt(x$y_hat*(1-x$y_hat/4e7))
x %>% ggplot(aes(y=residuo, x=t)) +
  geom_point()
x %>% ggplot(aes(x=residuo)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(
    fun = dnorm,
    lwd = 1, 
    col = 'red'
  )

library(car)
library(forecast)

qqPlot(x$residuo)

x$residuo %>% ggAcf()
x$residuo %>% ggPacf()

residuo.diff <- x$real %>%
  log1p() %>% 
  diff() %>% 
  diff(lag = 7)
data.frame(residuo=residuo.diff) %>% ggplot(aes(x=residuo)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(
    fun = dnorm,
    lwd = 1, 
    col = 'red'
  )

qqPlot(residuo.diff)

residuo.diff %>% ggAcf()
residuo.diff %>% ggPacf()

