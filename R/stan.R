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
library(cmdstanr)
library('COVID19')

library(shinystan)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)
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
theta.true <- c(-0.2)
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
p <- 10
pop <- df$population
y <- diff(df$deaths)
n <- length(y)
y <- y[1:(n-192)]
y <- pmax(y, 0)
n <- length(y)

data.frame(
  x = 1:n,
  deaths = y
) %>% ggplot(aes(x = x)) +
  geom_point(aes(y = deaths), alpha = 0.3)
n_ondas <- 5


# Ajuste:
# 
# mod <- cmdstan_model('./modelo_stan_tendencia.stan')
mod <- cmdstan_model('./minimo_erro.stan')

## Definição dos valores iniciais e dos dados para serem passados para o modelo
init <- list(
  b = array(c(5), dim = n_ondas),
  d = array(c(sum(y) / n_ondas), dim = n_ondas),
  e_ = array((n - 100) / n_ondas , dim = n_ondas),
  # theta = array(c(0), dim = 0),
  phi = array(c(0), dim = p),
  sigma = prec.true,
  s = array(1, dim = 7)
)

data <- list(
  N = n,
  n_ondas = n_ondas,
  populacao = pop[1],
  daily_deaths = y,
  t = 1:n,
  p = p,
  q = 0
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

opt$summary(variables = c(
  'd',
  'b',
  'e_cumsum',
  'sigma',
  's_normalized'
)) %>% print(n=30)

# fit.mcmc <- mod$sample(
#   data = data,
#   step_size = 0.01,
#   seed = 42,
#   init = opt,
#   chains = 4,
#   parallel_chains = 4,
#   adapt_delta = 0.99,
#   iter_warmup = 1000,
#   iter_sampling = 1500
# )
# fit.mcmc %>% launch_shinystan()

fit.la <- mod$laplace(
  data = data.opt,
  # step_size = 0.01,
  seed = 42,
  init = opt
  # mode = opt
  # chains = 1,
  # adapt_delta = 0.99,
  # iter_warmup = 10,
  # iter_sampling = 10
)
fit.la$summary(variables = c(
  'd',
  'b',
  'e_cumsum',
  'sigma',
  's_normalized'
)) %>% print(n=30)
opt.completo <- mod$optimize(
  data = data,
  seed = 42,
  init = list(init),
  jacobian = T,
  algorithm = 'bfgs',
  init_alpha = 1e-5
)
opt.completo$summary(variables = c(
  'd',
  'b',
  'e_',
  'phi',
  'sigma',
  's_normalized'
)) %>% print(n=30)

fit.la <- mod$laplace(
  data = data,
  # step_size = 0.01,
  seed = 42,
  init = opt.completo,
  mode = opt.completo,
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
  'phi',
  'sigma',
  's_normalized'
)) %>% print(n=40)


# Leitura das amostras geradas das distribuições a posteriori.
y_gen <- fit.la$draws('y_gen')[, 1:n]
mu.posterior <- fit.la$draws('np')[, 1:n]
eta.posterior <- fit.la$draws('eta_t')[, 1:n]
dim(y_gen)

mu.hat <- mu.posterior %>% apply(2, mean)
eta.hat <- eta.posterior %>% apply(2, mean)

df.test <- data.frame(estimate = mu.hat, real = y)

# Gráficos das distribuições a posteriori do modelo

## Valor real x componentes deterministicos
df.test %>%
  ggplot(aes(x = real, y = estimate)) +
  geom_point() +
  geom_abline()

df.test %>%
  ggplot(aes(x=1:n)) +
  geom_line(aes(y = estimate)) +
  geom_point(aes(y = real))


y_gen.summary <- y_gen %>% apply(2, summary)
typeof(y_gen.summary)
df.test <- cbind(df.test, t(data.frame(y_gen.summary)))
colnames(df.test)
df.test %>%
  ggplot(aes(x = 1:n)) +
  geom_ribbon(aes(ymin = `1st Qu.`, ymax=`3rd Qu.`, color = 'Intervalo de 50% de credibilidade')) +
  geom_point(aes(y = real)) +
  geom_line(aes(y = estimate, color = 'Componente determinístico')) +
  geom_line(aes(y = Mean, color = 'Média dos valores gerados')) +
  geom_line(aes(y = Median, color = 'Mediana dos valores gerados'))


(y / exp(eta.hat)) %>% ggAcf()
(y / exp(eta.hat)) %>% ggPacf()

((df.test$real >= df.test$`1st Qu.`) & (df.test$real <= df.test$`3rd Qu.`)) %>% mean()
