# remove.packages("rstan")
# if (file.exists(".RData")) file.remove(".RData")
# 
# # find the dependencies and drop shinystan from the list
# install.packages("remotes")
# deps <- remotes::package_deps("rstan", dependencies = TRUE)
# deps <- deps[!deps$package == "shinystan", ] 
# 
# # then this should install the dependencies
# update(deps) 
# 
# # after that then try installing rstan without forcing it to install dependencies
# install.packages("rstan", dependencies = FALSE)
# 
# # Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
# # install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
# 
# example(stan_model, package = "rstan", run.dontrun = TRUE)

library("rstan")
source('./tendencia.R')
library("bayesplot")
library("ggplot2")
library(dplyr)
library("parallel")
library(tidyr)
library(data.table)
library(FAdist)
library(car)
library(forecast)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)
# 
# 
dados <- read.csv('./dados/estados/dados S╞o Paulo .csv') # covid19(country = c('Brazil'), level = 1)
dados %>% 
  ggplot(aes(y = c(0, diff(deaths)), x = date)) +
  geom_point(alpha = 0.3)
# 
# # x <- x[1:275,]
# # x$deaths %>% plot()
# fit <- estima_tendencia(x$deaths, populacao, params = T)
# params <- fit$params
# n_ondas <- length(params) / 3
# t <- fit$tendencia
# x$tendencia <- t
# x %>% ggplot(aes(x = as.Date(date))) +
#   geom_point(aes(y = deaths), alpha = 0.3) +
#   geom_line(aes(y = tendencia, group = 1, colour = 'Estimativa'))
#   
# data.frame(
#   x = as.Date(x$date)[2:nrow(x)],
#   deaths = x$deaths %>% diff(),
#   tendencia = diff(x$tendencia)
# ) %>% ggplot(aes(x = x)) +
#   geom_point(aes(y = deaths), alpha = 0.3) +
#   geom_line(aes(y = tendencia, group = 1, colour = 'Estimativa'))
# b.fit <- -params[seq(1, n_ondas * 3, by = 3)]
# d.fit <- params[seq(2, n_ondas * 3, by = 3)]
# d.fit <- d.fit / sum(d.fit)
# e.fit <- params[seq(3, n_ondas * 3, by = 3)]

n_chains <- 4
set.seed(42)

# Geração dos dados
s.true <- c(2, 1, 2, 1, 1, 0.2, 0.3)
s.true <- s.true / mean(s.true)

set.seed(42)

p <- 0
q <- 0
n_ondas <- 2
pop <- 4e7
d.true <- c(2e4, 3e4)
e.true <- c(160, 500)
b.true <- c(3, 20)
theta.true <- c(-0.6, 0, 0, 0, 0, 0, -0.4)
prec.true <- 50
P <- 0
Q <- 1
source('./gera_dados.R')
dados <- gera.dados.negbin(
  d.true,
  e.true,
  b.true,
  s.true,
  theta.true,
  c(),
  1200,
  pop,
  prec.true
)


y <- dados$y
mu <- dados$mu
eta <- dados$eta
eps <- dados$epsilon
# eta <- dados$eta
# p <- dados$p
# eps <- dados$epsilon
# plot(eps)
# plot(eta)
# plot(log((mu / pop) / (1 - mu/pop)))
# plot(mu)
# plot(p)
print(sum(y))
print(sum(d.true))

# hist(rbbinom(10000, pop, 1e8 * 0.0002, 1e8 *(1 - 0.0002)))
data.frame(
  x = 1:1200,
  deaths = y,
  media = mu
) %>% ggplot(aes(x = x)) +
  geom_point(aes(y = deaths), alpha = 0.3) #+
  # geom_line(aes(y = mu, group = 1, colour = 'Média'))
# source('./tendencia.R')
# fit <- estima_tendencia(
#   cumsum(y[seq(7, length(y), 7)]),
#   pop,
#   params = T,
#   param = 8,
#   weekly = F,
#   wcmethod = 'kriston_method',
#   dates=(1:1200)[seq(2, length(y), 7)],
#   loss = bbinomial.neg.loglik.loss
# )
# params <- fit$params
# print(params)
# n_ondas <- (length(params) - 1) / 3
# 
# b.fit <- -params[seq(1, n_ondas * 3, by = 3)]
# d.fit <- params[seq(2, n_ondas * 3, by = 3)]
# e.fit <- params[seq(3, n_ondas * 3, by = 3)]
# 
# (b.fit - b.true) / b.true
# (e.fit - e.true) / e.true
# (d.fit - d.true) / d.true
# d.fit
# 
# 
# data.frame(
#   x = 1:1200,
#   deaths = y,
#   tendencia = c(0, diff(fit$tendencia))
# ) %>% ggplot(aes(x = x)) +
#   geom_point(aes(y = deaths), alpha = 0.3) +
#   geom_line(aes(y = tendencia, group = 1, colour = 'Estimativa'))

# plot(1:1200, y)
# plot(1:1200, mu)
# plot((y - mu) / sqrt(mu + mu^2 / prec.true))
# 
# library(dplyr)
# library(forecast)
# set.seed(42)
# residuo.diff <- rpois(100, 100) %>%
#   log1p() %>%
#   diff() %>%
#   diff(lag = 7)
# 
# residuo.diff %>% ggAcf()
# residuo.diff %>% ggPacf()

(log(pmax(y, 0.5)) - log(mu))[100:600] %>% ggAcf()
(log(pmax(y, 0.5)) - log(mu))[100:600] %>% ggPacf()
(log(pmax(y, 0.5)) - log(mu))[100:600] %>% plot()

# Ajuste:

init <- 1:n_chains %>% lapply(FUN = function(i) list(
  b = array(c(5, 5), dim = n_ondas),
  d = array(c(sum(y) / 2, sum(y) / 2), dim = n_ondas),
  e_ = array(c(300, 600), dim = n_ondas),
  theta = array(c(-0.3), dim = q),
  THETA = array(c(-0.3), dim = Q),
  prec = prec.true
))
start <- Sys.time()
fit.stan <- stan(
  file = 'modelo_stan_tendencia.stan',
  pars = c('d', 'b', 's_normalized', 'e_cumsum', 'theta', 'phi', 'sigma', 'THETA', 'PHI'),
  data = list(
    N = 1200,
    n_ondas = n_ondas,
    populacao = 4e7,
    daily_deaths = y,
    t = 1:1200,
    p = p,
    q = q,
    S = 7,
    P = P,
    Q = Q
  ),
  iter = 5000,
  chains = n_chains,
  thin = 3,
  init = init,
  warmup = 2000,
  control = list(
    adapt_delta = 0.99
  )
)
end <- Sys.time()
print(end - start)

print(fit.stan)
summary(fit.stan, pars = c(c('d', 'b', 's_normalized', 'e_cumsum', 'theta', 'THETA', 'sigma')))$summary[, c(4, 6, 8)]
saveRDS(fit.stan, 'fit.stan_2ondas_teste_garma(0,1)X(0, 1)_negbinom.rds')
fit.stan <- readRDS('fit.stan_2ondas_teste_garma(0,7)_negbinom.rds')
posterior <- as.array(fit.stan)
np <- nuts_params(fit.stan)
mcmc_trace(posterior, pars=vars(param_range('d', 1:n_ondas)), np = np) + 
  xlab("Post-warmup iteration")
mcmc_trace(posterior, pars=vars(param_range('e_cumsum', 1:n_ondas)), np = np) + 
  xlab("Post-warmup iteration")
mcmc_trace(posterior, pars=vars(param_range('b', 1:n_ondas)), np = np) + 
  xlab("Post-warmup iteration")
mcmc_trace(posterior, pars=vars(param_range('s_normalized', 1:7)), np = np) + 
  xlab("Post-warmup iteration")
mcmc_trace(posterior, pars=vars(param_range('theta', 1:q)), np = np) + 
  xlab("Post-warmup iteration")

mcmc_acf(posterior, pars=vars(param_range('theta', 1:q)))
mcmc_acf(posterior, pars=vars(param_range('d', 1:n_ondas)))
mcmc_acf(posterior, pars=vars(param_range('e_cumsum', 1:n_ondas)))
mcmc_acf(posterior, pars=vars(param_range('b', 1:n_ondas)))

# plot(fit.stan)
# plot(fit.stan, pars = c('d'))
# abline(v = 27500, col = 'red')

sample <- rstan::extract(fit.stan, pars = c('d', 'b', 's_normalized', 'e_cumsum'))
sigma.sample <- rstan::extract(fit.stan, pars = c('sigma'))$sigma
d.sample <- sample$d
b.sample <- sample$b
s.sample <- sample$s_normalized
e.sample <- sample$e_cumsum
# e.sample <- t(apply(e_.sample, 1, cumsum))
n_ondas <- 2
deaths.marg <- mclapply(1:length(y), function(t) {
  # p <- d / (1 + (x / e) ^ -b)
  # t <- 2
  p <- 0
  ondas <- data.frame(i=1:nrow(d.sample))
  for (i in 1:n_ondas) {
    onda <- d.sample[, i] * dllog(t, 1 / b.sample[, i], log(e.sample[, i])) * s.sample[, ((t - 1) %% 7) + 1]
    ondas[i] <- onda
    p <- p + onda
  }
  colnames(ondas) <- 1:n_ondas
  ondas.summary <- lapply(ondas, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))) %>% as.data.frame()
  ondas.means <- lapply(ondas, function(x) c("Mean"=mean(x))) %>% as.data.frame()
  ondas.summary <- rbind(ondas.summary, ondas.means)
  ondas.summary <- cbind(indice=rownames(ondas.summary), ondas.summary)
  ondas.summary <- ondas.summary %>%
    pivot_wider(values_from=2:(n_ondas+1), names_from = 'indice')
  deaths <- p
  # deaths <- escala* exp(amostra$`Theta1 for idx`) / (1 + (x / exp(amostra$`Theta3 for idx`)) ^ -exp(amostra$`Theta2 for idx`))
  qs <- quantile(deaths, probs=c(0.025, 0.5, 0.975))
  qs['Mean'] <- mean(deaths)
  qs <- c(ondas.summary[1,] %>% as.vector() %>% unlist(), qs)
  return(qs)
})
# View(deaths.marg)

x$idx <- 1:nrow(x)
deaths.marg <- as.data.frame(do.call(rbind, deaths.marg))

data.frame(deaths=y, Mean = deaths.marg$Mean, idx = 1:length(y)) %>%
  ggplot(aes(x=idx, y=deaths)) +
  geom_line(aes(y=Mean, colour = factor("Estimativa")), alpha=0.5) +
  geom_point(alpha=0.1) +
  # geom_ribbon(aes(ymin = wave2.5.,
  #                 ymax = wave97.5.,
  #                 colour=Onda, group=Onda, fill=Onda), alpha=0) +
  scale_colour_manual(values = rainbow(n_ondas + 1)) +
  coord_cartesian(xlim=c(495, 505))

deaths.marg <- deaths.marg %>%
  mutate(across(1:length(deaths.marg), cumsum))
x <- data.frame(deaths=cumsum(y), idx=1:length(y), date=1:length(y), population=4e7)
# plot(deaths.marg$Mean)
x <- cbind(deaths.marg, x[, c('date','deaths', 'idx', 'population')])

x_diff <- data.frame(
  'date'= x$date[2:nrow(x)],
  'deaths'=diff(x$deaths),
  'idx'=2:(nrow(x)),
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

# x$Mean %>% plot()

# x %>% ggplot(aes(x = as.Date(date), y = deaths)) +
#   geom_point(alpha=0.03) +
#   geom_line(aes(y=Mean, colour = 'Estimativa')) +
#   geom_ribbon(aes(ymin = `2.5%`,
#                   ymax = `97.5%`,
#                   colour='Estimativa',
#                   group='Estimativa',
#                   fill='Estimativa'), alpha=0.2)

df %>% ggplot(aes(x = as.Date(date), y = deaths)) +
  geom_point(alpha=0.03) +
  geom_line(aes(y=Mean, colour = 'Estimativa')) +
  geom_line(aes(y=`waveMean`, colour = Onda, group = Onda)) +
  geom_ribbon(aes(ymin=`wave2.5%`,
                  ymax=`wave97.5%`,
                  colour = Onda, fill=Onda, group=Onda), alpha = 0.1) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`,
                  colour='Estimativa', group='Estimativa', fill='Estimativa'), alpha=0.2) +
  scale_colour_manual(values = rainbow(n_ondas + 1))


df_diff %>% ggplot(aes(x=idx, y=deaths)) +
  geom_line(aes(y=Mean, colour = factor("Estimativa")), alpha=0.5) +
  geom_line(aes(y=waveMean, colour = Onda, group = Onda), alpha=0.2) +
  geom_ribbon(aes(ymin = min,
                  ymax = max,
                  fill=factor('Estimativa'))) +
  geom_point(alpha=0.1) +
  # geom_ribbon(aes(ymin = wave2.5.,
  #                 ymax = wave97.5.,
  #                 colour=Onda, group=Onda, fill=Onda), alpha=0) +
  scale_colour_manual(values = rainbow(n_ondas + 1))
((df$deaths >= df$`2.5%`) & (df$deaths <= df$`97.5%`)) %>% mean()
((df_diff$deaths >= df_diff$min) & (df_diff$deaths <= df_diff$max)) %>% mean()

x_res <- data.frame(real = df_diff$deaths,
                    estimativa = df_diff$Mean)
x_res$residuo <- (x_res$real - x_res$estimativa) / sqrt(x_res$estimativa + x_res$estimativa ^ 2 / mean(sigma.sample))
x_res %>% ggplot(aes(y=residuo, x=1:nrow(x_res))) +
  geom_point()
x_res %>% ggplot(aes(x=residuo)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(
    fun = dnorm,
    lwd = 1, 
    col = 'red'
  )

library(car)
library(forecast)

qqPlot(x_res$residuo)

x_res$residuo %>% ggAcf()
x_res$residuo %>% ggPacf()

residuo.diff <- x_res$estimativa %>%
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

pairs(fit.stan, pars = paste('d[', 1:n_ondas, ']', sep=''))
pairs(fit.stan, pars = paste('e_[', 1:10, ']', sep=''))
pairs(fit.stan, pars = paste('b[', 1:10, ']', sep=''))


sample <- rstan::extract(fit.stan)
cors <- d.sample %>% cor() %>% round(2)
library(reshape2)

melted_cors <- melt(cors)

melted_cors %>% ggplot(aes(x=factor(Var1), y=factor(Var2), fill=value, label=value)) + 
  geom_tile() +
  geom_text() +
  scale_x_discrete() +
  scale_y_discrete(limits=factor(n_ondas:1)) +
  scale_fill_gradient2(low = "blue1", high = "green4", mid = "cyan", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle('d')

cors <- sample[['e_']] %>% cor() %>% round(2)
library(reshape2)

melted_cors <- melt(cors)

melted_cors %>% ggplot(aes(x=factor(Var1), y=factor(Var2), fill=value, label=value)) + 
  geom_tile() +
  geom_text() +
  scale_x_discrete() +
  scale_y_discrete(limits=factor(10:1)) +
  scale_fill_gradient2(low = "blue1", high = "green4", mid = "cyan", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle('e')

cors <- sample[['b']] %>% cor() %>% round(2)
library(reshape2)

melted_cors <- melt(cors)

melted_cors %>% ggplot(aes(x=factor(Var1), y=factor(Var2), fill=value, label=value)) + 
  geom_tile() +
  geom_text() +
  scale_x_discrete() +
  scale_y_discrete(limits=factor(10:1)) +
  scale_fill_gradient2(low = "blue1", high = "green4", mid = "cyan", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle('b')