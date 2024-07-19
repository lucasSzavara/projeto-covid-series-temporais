library(drc)
library(dplyr)
library(lubridate)
library(nloptr)
source('./likelihoods.R')
source('./wave_counting.R')  
  
chutes_iniciais <- function(serie, N, minimos) {
  ip <- c()
  for (i in 1:N) {
    if (N == 1) {
      yi <- serie
      ti <- 1:length(serie)
    } else if(i == N) {
      # pode pegar só o fim N = 1
      yi <- serie[as.integer(minimos[i]):length(serie)] - serie[as.integer(minimos[i])-1]
      ti <- as.integer(minimos[i]):length(serie)
    } else {
      valor_inicial <- 0
      if (minimos[i] != 1) {
        valor_inicial <- serie[as.integer(minimos[i])-1]
      }
      yi <- serie[as.integer(minimos[i]):as.integer(minimos[i+1]+1)]- valor_inicial
      ti <- as.integer(minimos[i]):as.integer(minimos[i+1]+1)
    }
    modeloi <- drm(yi~ti, fct=LL.3())
    ipi <- t(as.data.frame(getInitial(modeloi)))
    colnames(ipi) <- paste(substr(colnames(ipi),1,1), i, sep='')
    ip <- cbind(ip, ipi)
  }
  return(ip)
}

ajusta_modelo <- function(serie,
                          N,
                          chutes,
                          pop,
                          loss=binomial_loglik_loss) {
  tempo <- 1:length(serie)
  if (N == 0) {
    fit <- drm(serie~tempo, fct=LL.3())
    return(list(param=fit$coefficients,
                loglik=logLik(fit)[1],
                # file:///home/lucas/Downloads/b97636.pdf pg 90: correção para amostras pequenas, nesse caso, se existir mais de 8 ondas
                aic=AIC(fit),
                bic=BIC(fit)
                ))
  }
  factors <- sapply(1:N, FUN=function(i){paste('I(d',i,'/(1+exp(b',i,'*(log(x)-log(e',i,')))))', sep='')})
  model_formula <- reformulate(termlabels = factors, response = 'y')
  dados <- data.frame(y=serie, x=tempo)
  fit <- nloptr(
    x0=chutes,
    eval_f=loss,
    # talvez trocar o algoritmo por NLOPT_LN_BOBYQA ou usar algum algoritmo de otimização global?
    opts = list("algorithm" = "NLOPT_LN_SBPLX",
                "xtol_rel"=1.0e-8,
                "print_level"=0,
                "maxeval"=5000),
    y = serie,
    x = tempo,
    N = pop
  )
  names(fit$solution) <- names(chutes)
  loglik <- -fit$eval_f(fit$solution)
  k <- N*3
  return(list(param=fit$solution,
              loglik=loglik,
              aic=2*k - 2*loglik,
              bic=k*log(length(serie)) - 2*loglik
              ))
}

estima_tendencia <- function(serie,
                             pop,
                             params=F,
                             weekly=F,
                             param=50,
                             wcmethod='splines_method',
                             dates=c(), 
                             loss=binomial_loglik_loss) {
  wave_counts <- wave_count(wcmethod, serie, param, weekly, dates)
  obj <- wave_counts$obj
  minimos <- wave_counts$minimos
  minimos <- remove_empty_waves(serie, minimos)
  t <- 1:length(serie)
  
  fit <- ajusta_modelo(serie, 
                       length(minimos), 
                       chutes_iniciais(serie, length(minimos), minimos)[1,],
                       pop,
                       loss=loss
                       )
  loglik <- fit$loglik
  aic <- fit$aic
  bic <- fit$bic
  fit <- fit$param
  if (params) {
    # Como manter estavel para quando ajusto só pelo drm e quando ajusto pelo nls??
    if (length(minimos) > 0) {
      chute_inicial <- chutes_iniciais(serie, length(minimos), minimos)
      return(list(tendencia=calcula_tend(fit, t)$n,
                  params=fit,
                  chutes=chute_inicial,
                  loglik=loglik,
                  aic=aic,
                  bic=bic,
                  obj=obj))
    }
    ipi <- t(as.data.frame(fit))
    colnames(ipi) <- paste(substr(colnames(ipi),1,1), 1, sep='')
    return(list(tendencia=calcula_tend(fit, t)$n,
                params=ipi['fit',],
                chutes=ipi['fit',],
                loglik=loglik,
                aic=aic,
                bic=bic,
                obj=obj))
  }
  return(calcula_tend(fit, t)$n)
}


calcula_tend <- function(params, t) {
  ondas <- length(params) / 3
  result <- data.frame(t=t, n=0)
  for(i in 1:ondas) {
    d <- params[paste('d',i,sep='')]
    b <- params[paste('b',i,sep='')]
    e <- params[paste('e',i,sep='')]
    result$n <- result$n + (d / (1+exp(b*(log(t)-log(e)))))
  }
  return(result)
}



# serie <- df$deaths
# fit <- estima_tendencia(serie, params=T)
# 
# fit$params
# serie[is.na(serie)] <- 0
# # cálculo da série diária
# y <- c(1, diff(serie))
# 
# # Cálculo das splines cúbicas
# spline_fit <- smooth.spline(y, df=40)
# poly <- SmoothSplineAsPiecePoly(spline_fit)
# # Maximos e minimos das splines
# zeros  <- solve(poly, deriv=1)
# minimos_ <- zeros[predict(poly, zeros, deriv=2)>0]
# minimos <- c()
# for (i in 1:(length(minimos_) - 1)) {
#   if(minimos_[i+1] - minimos_[i] > 90) {
#     minimos <- c(minimos, minimos_[i])
#   }
# }

# N <- length(minimos)
# factors <- sapply(1:N, FUN=function(i){paste('I(d',i,'/(1+exp(b',i,'*(log(x)-e',i,'))))', sep='')})
# model_formula <- reformulate(termlabels = factors, response = 'y')
# t <- 1:length(serie)
# ip <- c()
# for (i in 1:N) {
#   if(i == N) {
#     yi <- serie[as.integer(minimos[i]):length(serie)] - serie[as.integer(minimos[i])-1]
#     ti <- as.integer(minimos[i]):length(serie)
#   } else {
#     yi <- serie[as.integer(minimos[i]):as.integer(minimos[i+1]+1)]- serie[as.integer(minimos[i])-1]
#     ti <- as.integer(minimos[i]):as.integer(minimos[i+1]+1)
#   }
#   modeloi <- drm(yi~ti, fct=LL2.3())
#   ipi <- t(as.data.frame(modeloi$coefficients))
#   colnames(ipi) <- paste(substr(colnames(ipi),1,1), i, sep='')
#   ip <- cbind(ip, ipi)
# }
# dados <- data.frame(y=serie, x=t)
# ctrl <- nls.control(maxiter = 500, warnOnly=T)
# 
# fit <- nls(model_formula, dados, start=ip[1,], control = ctrl, algorithm='port')