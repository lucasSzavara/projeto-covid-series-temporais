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
