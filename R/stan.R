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
library(cmdstanr)
library('COVID19')
library(ggpubr)
library(shinystan)

options(mc.cores = 4)

# Salvar dados de cidades do Brasil em arquivos separados
x <- covid19(country=c('Brazil'), level=3, verbose=F, vintage = "2023-09-30")
df <- x %>%
  filter(administrative_area_level_3 == 'São Paulo')
rm(x)
plot(df$deaths)



set.seed(42)

# Geração dos dados
s.true <- c(2, 1, 2, 1, 1, 0.2, 0.3)
s.true <- s.true / mean(s.true)


n_ondas <- 2
pop <- 143*1e5
d.true <- c(2e4, 3e4)
e.true <- c(160, 500)
b.true <- c(3, 20)
theta.true <- c()
phi.true <- c(0.5, 0.1)
prec.true <- 15
P <- 0
Q <- 1
n <- 700
source('./gera_dados.R')
set.seed(100)
dados <- gera.dados.negbin(
  d.true,
  e.true,
  b.true,
  s.true,
  theta.true,
  phi.true,
  n,
  pop,
  prec.true
)


y <- dados$y
mu <- dados$mu
eta <- dados$eta
eps <- dados$epsilon

print(sum(y))
print(sum(d.true))


# Seleção dos dados para serem passados para o modelo

# p <- phi.true %>% length()
# q <- phi.true %>% length()
q <- 0
p <- 14
pop <- df$population
y <- diff(df$deaths)
dates <- df$date[2:(dim(df)[1])]
n <- length(y)
y <- y[1:(n-192)]
y <- pmax(y, 0)
y <- y[1:600]
n <- length(y)

data.frame(
  x = 1:n,
  deaths = y
) %>% ggplot(aes(x = x)) +
  geom_point(aes(y = deaths), alpha = 0.3)
n_ondas <- 3


# library(zoo)
# y_roll = rollapply(y[seq(5, n, 7)], FUN = mean, width = 5)
# var_roll = rollapply(y[seq(5, n, 7)], FUN = var, width = 5)
# p0_roll = rollapply(y[seq(2, n, 7)] == 0, FUN = mean, width = 7)
#
# data.frame(y = y_roll, logit = logitlink(p0_roll)) %>%
#     ggplot(aes(x = log(y), y = logit)) +
#     geom_point() +
#     geom_jitter()
#
#
# data.frame(y = y_roll, var = var_roll) %>%
#     ggplot(aes(x = log(y), y = y ^ 2 / (var_roll - y))) +
#     geom_point() +
#     geom_jitter() +
#     coord_cartesian(ylim=c(0, 100))


# Ajuste:
#
# mod <- cmdstan_model('./modelo_stan_tendencia.stan')
mod <- cmdstan_model('./minimo_erro.stan')

p <- 7

## Definição dos valores iniciais e dos dados para serem passados para o modelo
init <- list(
  b = array(c(5), dim = n_ondas),
  d = array(c(sum(y) / n_ondas), dim = n_ondas),
  e_ = array((n - 100) / n_ondas , dim = n_ondas),
  # theta = array(c(0), dim = 0),
  phi = array(0, dim = p),
  sigma = prec.true,
  s = array(1, dim = 7),
  nu = 0.8
)

data <- list(
  N = n,
  n_ondas = n_ondas,
  populacao = pop[1],
  daily_deaths = y,
  t = 1:n,
  p = p,
  q = 0
  # nu = 1
)

data.opt <- list(
  N = n,
  n_ondas = n_ondas,
  populacao = pop[1],
  daily_deaths = y,
  t = 1:n,
  p = 0,
  q = 0
)
#
init.opt <- list(
  b = array(c(5), dim = n_ondas),
  d = array(c(sum(y) / n_ondas), dim = n_ondas),
  e_ = array((n - 100) / n_ondas , dim = n_ondas),
  # theta = array(c(0), dim = 0),
  phi = array(c(0), dim = 0),
  sigma = prec.true,
  s = array(1, dim = 7)
)

opt <- mod$optimize(
  data = data.opt,
  seed = 42,
  init = list(init.opt),
  jacobian = T
)
#
opt$summary(variables = c(
  'd',
  'b',
  'e_cumsum',
  # 'sigma',
  's_normalized'
)) %>% print(n=30)

opt.completo <- mod$optimize(
  data = data,
  seed = 42,
  init = opt,
  jacobian = T,
  algorithm = 'bfgs',
  init_alpha = 1e-10
)
opt.completo$summary(variables = c(
  'd',
  'b',
  'e_',
  # 'phi',
  's_normalized',
  'alpha_sigma',
  # 'beta_sigma',
  'beta',
  'alpha'
)) %>% print(n=50)

fit.la <- mod$laplace(
  data = data,
  # step_size = 0.01,
  seed = 42,
  init = opt,
  # mode = opt,
  jacobian = T
  # chains = 1,
  # adapt_delta = 0.99,
  # iter_warmup = 10,
  # iter_sampling = 10
)
fit.la$summary(variables = c(
  'd',
  'b',
  'e_cumsum',
  # 'phi',
  'alpha_sigma',
  # 'beta_sigma',
  's_normalized',
  # 'sigma',
  'beta',
  'nu',
  'alpha'
)) %>% print(n=40)


# Leitura das amostras geradas das distribuições a posteriori.
y_gen <- fit.la$draws('y_gen')[, 1:n]
res_norm <- fit.la$draws('residuo_quantil_normalizado')[, 1:n]
sigma <- fit.la$draws('sigma')[, 1:n]
p0 <- fit.la$draws('p0')[, 1:n]
b <- fit.la$draws('b_res')[, 1:n]
eps <- fit.la$draws()
# z_score <- fit.la$draws('z_score')[, 1:n]
mu.posterior <- fit.la$draws('np')[, 1:n]
eta.posterior <- fit.la$draws('eta_t')[, 1:n]
# sigma.posterior <- fit.la$draws('sigma')

mu.hat <- mu.posterior %>% apply(2, mean)
eta.hat <- eta.posterior %>% apply(2, mean)
sigma.hat <- sigma %>% apply(2, mean)
res_norm.mean <- res_norm %>% apply(2, mean)
p0.mean <- p0 %>% apply(2, mean)
b.mean <- b %>% apply(2, mean)
dim(y_gen)

ppc_stat_grouped(y, y_gen, stat = 'sum', group = rep(1:7, length.out=n))
ppc_stat_grouped(y, y_gen, stat = function(x) mean(x == 0), group = rep(1:7, length.out=n))
# ppc_stat_grouped(y, y_gen, stat = function(x) var(x == 0), group = rep(1:7, length.out=n))
ppc_stat_grouped(y, y_gen, stat = 'max', group = rep(1:7, length.out=n)) +
    scale_x_continuous(n.breaks = 10)
ppc_dens_overlay_grouped(y, y_gen[sample(1:1000, 50), ], group = rep(1:7, length.out=n))
ppc_rootogram(y, y_gen, prob = 0.5)
ppc_stat(y, y_gen, stat = 'max') +
    scale_x_continuous(n.breaks = 10)

ppc_error_hist_grouped(y, y_gen[sample(1:1000, 5), ], group = rep(1:7, length.out=n))
ppc_error_scatter(y, y_gen[sample(1:1000, 12), ])

# library(arules)
# grupos <- paste(rep(1:7, length.out=n), cut(exp(eta.hat), breaks = c(seq(0, 90, 30), 700), include.lowest=T))
# ppc_stat_grouped(y, y_gen, stat = 'sum', group = grupos)
# ppc_stat_grouped(y, y_gen, stat = 'mean', group = grupos)
# ppc_stat_grouped(y, y_gen, stat = function(x) sum(x == 0), group = grupos)
# # ppc_stat_grouped(y, y_gen, stat = function(x) var(x == 0), group = grupos)
# ppc_stat_grouped(y, y_gen, stat = 'max', group = grupos) +
#     scale_x_continuous(n.breaks = 10)


df.test <- data.frame(estimate = mu.hat,
                      real = y,
                      full_estimate = exp(eta.hat),
                      res = res_norm.mean,
                      p0 = p0.mean,
                      dia = rep(1:7, length.out = n),
                      b = b.mean)

# Gráficos das distribuições a posteriori do modelo
x_int <- which(df.test$res > 2.5)
## Valor real x componentes deterministicos
df.test %>%
    ggplot(aes(x = real, y = estimate)) +
    geom_point(aes(color = res > 2.5)) +
    geom_text(aes(label = round(b, 2))) +
    geom_abline()

df.test %>%
    group_by(dia) %>%
    summarise(res = mean(res > 2.5)) %>%
    ggplot(aes(x = factor(dia), y = res)) +
    geom_bar(stat = 'identity')
df.test %>%
    ggplot(aes(x = factor(dia), y = res)) +
    geom_boxplot()

df.test %>%
    ggplot(aes(x = real, y = p0)) +
    geom_point(aes(color = res > 2.5)) +
    geom_abline()

df.test %>%
    ggplot(aes(x=1:n)) +
    geom_line(aes(y = estimate)) +
    # geom_text(aes(y = real, label = round(b, 2))) +
    geom_point(aes(y = real, color = res > 2.5)) +
    # coord_cartesian(xlim = c(100, 200)) +
    geom_vline(xintercept = x_int)


df.test %>%
    ggplot(aes(x=1:n)) +
    geom_line(aes(y = full_estimate)) +
    geom_text(aes(y = real, label = round(b, 1))) +
    geom_point(aes(y = real, color = res))




y_gen.summary <- y_gen %>% apply(2, function(x) quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975)))
typeof(y_gen.summary)
df.test <- cbind(df.test, t(data.frame(y_gen.summary)))
colnames(df.test)
df.test %>%
    ggplot(aes(x = 1:n)) +
    geom_point(aes(y = real)) +
    geom_ribbon(aes(
        ymin = `2.5%`,
        ymax=`97.5%`,
        fill = 'Intervalo de predição',
        color = 'Intervalo de predição',
        alpha = 0.5
    )) +
    # geom_line(aes(y = estimate, color = 'Componente determinístico')) +
    geom_line(aes(y = exp(eta.hat), color = 'Média de mu'))


(y / exp(eta.hat)) %>% ggAcf()
(y / exp(eta.hat)) %>% ggPacf()

df.test %>%
    ggplot(aes(x = 1:n, y = real / exp(eta.hat))) +
    geom_point()

set.seed(42)
n_res <- 10
res <- t(res_norm[sample(1:1000, n_res), ]) %>% as.data.frame()

colnames(res) <- paste('res', 1:n_res)
res$t <- 1:n

res[res == Inf] <- 8

r <- res %>% pivot_longer(cols =  paste('res', 1:n_res), names_to = 'observacao', values_to = 'residuo')
r %>%
    ggplot(aes(x = residuo)) +
    geom_histogram()
r %>%
    ggplot(aes(sample = residuo)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~observacao)
library(gamlss)
library(VGAM)

# norm <- rnorm(5000)
# ggplot(data.frame(sample=norm), aes(sample=sample)) +
#     stat_qq(distribution = qt, dparams = list(df=5)) +
#     stat_qq_line(distribution = qt, dparams = list(df=5))

plots <- list()
for (i in 1:n_res) {
    plots[[i]] <- wp(resid = r$residuo[r$observacao == paste('res', i)], ylim.all = 5)
}
ggarrange(plotlist = plots)

r %>%
    ggplot(aes(x = t, y = residuo)) +
    geom_point() +
    facet_wrap(~observacao)

plots <- list()
for (i in 1:n_res) {
    plots[[i]] <- r$residuo[r$observacao == paste('res', i)] %>%
        ggAcf()
}
ggarrange(plotlist = plots)


plots <- list()
for (i in 1:n_res) {
    plots[[i]] <- r$residuo[r$observacao == paste('res', i)] %>%
        ggPacf()
}

ggarrange(plotlist = plots)

# set.seed(42)
# n_res <- 10
# res <- t(z_score[sample(1:1000, n_res), ]) %>% as.data.frame()
#
# colnames(res) <- paste('res', 1:n_res)
# res$t <- 1:n
#
# r <- res %>% pivot_longer(cols =  paste('res', 1:n_res), names_to = 'observacao', values_to = 'residuo')
# r %>%
#     ggplot(aes(sample = residuo)) +
#     stat_qq() +
#     stat_qq_line() +
#     facet_wrap(~observacao)
#
# r %>%
#     ggplot(aes(x = t, y = residuo)) +
#     geom_point() +
#     facet_wrap(~observacao)
#
# plots <- list()
# for (i in 1:n_res) {
#     plots[[i]] <- r$residuo[r$observacao == paste('res', i)] %>%
#         ggAcf()
# }
# ggarrange(plotlist = plots)
#
#
# plots <- list()
# for (i in 1:n_res) {
#     plots[[i]] <- r$residuo[r$observacao == paste('res', i)] %>%
#         ggPacf()
# }
#
# ggarrange(plotlist = plots)

((df.test$real >= df.test$`2.5%`) & (df.test$real <= df.test$`97.5%`)) %>% mean()
((df.test$real >= df.test$`25%`) & (df.test$real <= df.test$`75%`)) %>% mean()
