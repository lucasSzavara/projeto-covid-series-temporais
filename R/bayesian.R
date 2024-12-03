update.packages( ask=FALSE, checkBuilt=TRUE )
library(COVID19)
library(dplyr)
library(INLA)
library(ggplot2)
library(drc)
library("parallel")
library(tidyr)
source('./tendencia.R')
# Set number of cores to use
options(mc.cores = 4)
theme_set(theme_bw())
x <- read.csv('./dados/estados/dados S╞o Paulo .csv') # covid19(country = c('Brazil'), level = 1)

x %>% ggplot(aes(x=date, y=deaths)) +
  geom_point()
x$idx <- 1:nrow(x)
mod_ <- drm(x$deaths~x$idx, fct=LL.3())
summary(mod_)

N <- x$population[1]
escala <- 1e4
rgeneric.logit.sums <- function(
    cmd=c("graph", "Q", "mu", "initial", "log.norm.const",
          "log.prior", "quit"),
    theta = NULL
) {
  # n: tamanho da série
  # N: população
  # source('./tendencia.R')
  envir = parent.env(environment())
  library(Matrix)
  
  graph = function() {
    return (Diagonal(n=n - 1, x=1))
  }
  
  Q = function() {
    np <- mu()
    Var <- np * (1 - np/(N/escala))
    # if (sum(Var < 0) > 0) stop('Var < 0????')
    # if (sum(Var == 0) > 0) stop('Var = 0????')
    # if (sum(Var == Inf) > 0) stop('Var = Inf????')
    
    prec <- 1/Var
    q <- prec * graph()
    # if(dim(graph())[1L] != dim(graph())[2L]) stop('graph??')
    # if(length(dim(q)) == 0) stop(sprintf('q vazio?? %f', prec))
    tryCatch(
      if(dim(q)[1L] != dim(q)[2L]) stop(paste(q, np, prec)),
      error=function(cnd) paste(q, np, prec)
    )
    warning(paste('Q:', length(q)))
    warning(paste('np:', length(np)))
    warning(paste('prec:', length(prec)))
    warning(paste('Var:', length(Var)))
    return(q)
  }
  
  mu = function() {
    Nn <- length(theta) / 3
    d <- exp(theta[seq(1, Nn * 3, by = 3)])
    b <- exp(theta[seq(2, Nn * 3, by = 3)])
    e <- exp(theta[seq(3, Nn * 3, by = 3)])
    e <- cumsum(e)
    
    # p <- d / (1 + (x / e) ^ -b)
    p <- 0
    for (i in 1:length(d)) {
      p <- p + d[i] / (1 + (t / e[i]) ^ -b[i])
    }
    return(diff(p))
  }
  
  log.norm.const = function() {
    return(numeric(0))
  }
  
  log.prior = function() {
    Nn <- length(theta) / 3
    media <- 1.5 * 2e5/(Nn * escala)
    variancia <- 1e4 ^ 2
    
    alpha <- media^2 / variancia
    beta <- media / variancia
    
    priori.d <- sum(dgamma(
      exp(theta[seq(1, Nn * 3, by = 3)]),
      alpha,
      rate = beta,
      log = T
    ))
    # priori.b <- sum(dnorm(theta[seq(2, Nn * 3, by=3)],
    #                       log(3) - b.sd/2, b.sd,
    #                       log=T))
    priori.b <- sum(dgamma(exp(theta[seq(2, Nn * 3, by = 3)]),
                             1.5, 1/100,
                             log = T))
    e <- exp(theta[seq(3, Nn * 3, by = 3)])
    # e <- diff(c(0, es))
    media <- (n - 100)/Nn
    variancia <- 30 ^ 2
    
    alpha <- media^2 / variancia
    beta <- media / variancia
    priori.e <- sum(dgamma(e,
                           alpha, rate = beta,
                           log = T))
    # priori.e <- 0
    # return(0)
    return(priori.d + priori.b + priori.e)
  }
  
  initial = function() {
    return(theta0)
  }
  
  quit = function() {
    return (invisible())
  }
  
  # will ensure that theta is always defined.
  if (!length(theta)) theta = initial()
  val = do.call(match.arg(cmd), args = list())
  return (val)
}
fit <- estima_tendencia(x$deaths / escala, x$population[1], params=T)
params <- fit$params
t <- fit$tendencia
x$tendencia <- t
x %>% ggplot(aes(x = date)) +
  geom_point(aes(y = deaths), alpha = 0.3) +
  geom_line(aes(y = tendencia * escala, group = 1, colour = 'Estimativa'))
Nn <- length(params) / 3
b.fit <- -params[seq(1, Nn * 3, by = 3)]
d.fit <- params[seq(2, Nn * 3, by = 3)]
e.fit <- params[seq(3, Nn * 3, by = 3)]
theta <- 1:(Nn*3)
ordenacao <- order(e.fit)
theta[seq(1, Nn * 3, by = 3)] <- log(d.fit[ordenacao])
theta[seq(2, Nn * 3, by = 3)] <- log(b.fit[ordenacao])
theta[seq(3, Nn * 3, by = 3)] <- log(diff(c(0, e.fit[ordenacao])))


model <- inla.rgeneric.define(rgeneric.logit.sums,
                              n = length(x$deaths),
                              t = 1:nrow(x),
                              escala = escala,
                              N = x$population[1],
                              theta0 = theta)
x_diario <- data.frame(deaths = diff(x$deaths),
                       idx = 1:(nrow(x) - 1))
mod <- inla(
  deaths / escala ~ -1 + f(idx, model=model),
  data = x_diario,
  verbose = T,
  control.inla = list(
    int.strategy = 'grid',
    strategy = 'laplace'
  ),
  safe = T,
  inla.mode='classic'
)
mod = inla.hyperpar(mod, verbose = T)
summary(mod)

amostrado <- mod$marginals.random$idx %>% lapply(inla.zmarginal)
amostra <- as.data.frame(inla.hyperpar.sample(10000, mod, improve.marginals = T, intern = T))[, -c(1)]
d <- exp(amostra[, seq(1, Nn * 3, by = 3)])
b <- exp(amostra[, seq(2, Nn * 3, by = 3)])
e <- exp(amostra[, seq(3, Nn * 3, by = 3)])
library(data.table)
setDT(e)[,names(e) := Reduce("+", e, accumulate = TRUE)]
summary(e)
e <- as.list(e)
summary(d * escala)
summary(b)
if (Nn == 1) {
  d <- data.frame(d=d)
  b <- data.frame(b=b)
  e <- data.frame(e=e)
}
# t <- x$idx
# i <- 4
# y <- d[1, i] / (1 + (t / e[1, i]) ^ -b[1, i])
# plot(t, y)
deaths.marg <- mclapply(x$idx, function(t) {
  # p <- d / (1 + (x / e) ^ -b)
  p <- 0
  ondas <- data.frame(i=1:10000)
  for (i in 1:Nn) {
    onda <- d[[i]] / (1 + (t / e[[i]]) ^ -b[[i]])
    ondas[i] <- escala * onda
    p <- p + onda
  }
  colnames(ondas) <- 1:Nn
  ondas.summary <- lapply(ondas, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))) %>% as.data.frame()
  ondas.means <- lapply(ondas, function(x) c("Mean"=mean(x))) %>% as.data.frame()
  ondas.summary <- rbind(ondas.summary, ondas.means)
  ondas.summary <- cbind(indice=rownames(ondas.summary), ondas.summary)
  ondas.summary <- ondas.summary %>%
    pivot_wider(values_from=2:(Nn+1), names_from = 'indice')
  deaths <- escala * p
  # deaths <- escala* exp(amostra$`Theta1 for idx`) / (1 + (x / exp(amostra$`Theta3 for idx`)) ^ -exp(amostra$`Theta2 for idx`))
  qs <- quantile(deaths, probs=c(0.025, 0.5, 0.975))
  qs['Mean'] <- mean(deaths)
  qs <- c(ondas.summary[1,] %>% as.vector() %>% unlist(), qs)
  return(qs)
})
deaths.marg <- as.data.frame(do.call(rbind, deaths.marg))
x <- cbind(deaths.marg, x[, c('date','deaths', 'idx', 'population')])
amostrado2 <- do.call(rbind, amostrado) %>%
  as.data.frame()
# x$mean <- amostrado2$mean %>% unlist() * escala
# x$quant0.975 <- amostrado2$quant0.975 %>% unlist() * escala
# x$quant0.025 <- amostrado2$quant0.025 %>% unlist() * escala
x_diff <- data.frame(
  'date'= x$date[2:nrow(x)],
  'deaths'=diff(x$deaths),
  'idx'=1:(nrow(x)-1),
  'Mean'=diff(x$Mean),
  # 'mean'=diff(x$mean),
  `max`=diff(x$`97.5%`),
  `min`=diff(x$`2.5%`)
)
x_diff_extra <- x %>%
  dplyr::select(starts_with('X')) %>% 
  lapply(function(x) diff(x)) %>%
  as.data.frame()
x_diff <- cbind(x_diff, x_diff_extra)

df_diff <- x_diff %>%
  pivot_longer(cols=starts_with('X'),
               names_to = c('Onda', 'Métrica'),
               names_pattern = c('X(.).(.*)'),
               values_to = 'Valor') %>% 
  pivot_wider(values_from = 'Valor',
              names_from = 'Métrica',
              id_cols = c(`min`, `max`, 'Mean', 'date', 'deaths', 'idx', 'Onda'),
              names_prefix = 'wave')

df <- x %>%
  pivot_longer(cols=starts_with('X'),
               names_to = c('Onda', 'Métrica'),
               names_pattern = c('X(.).(.*)'),
               values_to = 'Valor') %>% 
  pivot_wider(values_from = 'Valor',
              names_from = 'Métrica',
              id_cols = c(`2.5%`, `50%`, `97.5%`, 'Mean', 'date', 'deaths', 'idx', 'Onda'),
              names_prefix = 'wave')

df %>% ggplot(aes(x = as.Date(date), y = deaths)) +
  geom_point(alpha=0.03) +
  # geom_line(aes(y=mean, colour = 'Estimativa Agp')) +
  geom_line(aes(y=Mean, colour = 'Estimativa')) +
  geom_line(aes(y=`waveMean`, colour = Onda, group = Onda)) +
  geom_ribbon(aes(ymin=`wave2.5%`,
                  ymax=`wave97.5%`,
                  colour = Onda, fill=Onda, group=Onda), alpha = 0.2) +
  # geom_line(aes(y=`50%`, colour = 'mediana')) +
  # geom_ribbon(aes(ymin = quant0.025,
  #                 ymax = quant0.975,
  #                 colour='Estimativa Agp', group='Estimativa Agp', fill='Estimativa Agp'), alpha=0.2) +
  # geom_line(aes(y=`50%`, colour = 'mediana')) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`,
                  colour='Estimativa', group='Estimativa', fill='Estimativa'), alpha=0.2)

# ((df$deaths >= df$quant0.025) & (df$deaths <= df$quant0.975)) %>% mean()
((df$deaths >= df$`2.5%`) & (df$deaths <= df$`97.5%`)) %>% mean()

df_diff %>% ggplot(aes(x=date, y=deaths)) +
  geom_point(alpha=0.03) +
  # geom_line(aes(y=mean, colour = 'Estimativa Agp', group = 'Estimativa Agp')) +
  geom_line(aes(y=Mean, colour = 'Estimativa', group = 'Estimativa'), colour = 'black') +
  geom_line(aes(y=waveMean, colour = Onda, group = Onda)) #+
  # geom_ribbon(aes(ymin = min,
  #                 ymax = max,
  #                 colour='Estimativa Agp', group='Estimativa Agp', fill='Estimativa Agp'), alpha=0.2)
  # geom_ribbon(aes(ymin=wave2.5.,
  #                 ymax=wave97.5.,
  #                 colour = Onda, fill=Onda), alpha=0.05) +
  # geom_line(aes(y=`50%`, colour = 'mediana')) +
  # 
(df_diff$mean - df_diff$deaths) %>% summary()
a <- 1.5
b <- 1/100
n <- length(x$deaths)

media <- (n - 100)/Nn
variancia <- 35 ^ 2

alpha <- media^2 / variancia
beta <- media / variancia
curve(dgamma(x,
                 alpha, rate = beta,), to = 200)
diff(c(0, e.fit))
mean(e)
summary(e %>% as.data.frame())

media <- 1.5 * 2e5/Nn
variancia <- 1e4 ^ 2

alpha <- media^2 / variancia
beta <- media / variancia

curve(dgamma(
  x,
  alpha,
  rate = beta
), to=50000)
d.fit * escala
summary(d * escala)
