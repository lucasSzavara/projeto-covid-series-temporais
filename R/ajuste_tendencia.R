library(nloptr)
library(COVID19)

df <- covid19(country = c('Brazil'), level=1, verbose=F)

# cálculo da série diária
serie_diferenciada <- c(1, diff(df$deaths))
# cálculo da série diária diferenciada
serie_sem_tendencia <- diff(serie_diferenciada)
plot(serie_sem_tendencia)

# Cálculo da média móvel e pontos de inflexão
medias_moveis <- slider::slide_dbl(serie_sem_tendencia, mean, .before=15, .complete=TRUE)
dias <- as.numeric(df$date - df[1, 2])

ts <- which(medias_moveis == 0)
ts <- ts[ts > 30]
ts <- ts[ts < length(medias_moveis) - 30]
ts <- ts[2:(length(ts)-1)]

N <- length(ts) + 1

# Cálculo dos coeficientes como função do tempo
calcula_ps <- function(ps, t, rs) {
    soma <- 0
    for(i in 1:(length(ps) - 1)) {
      soma <- soma + ((ps[i+1]-ps[i])/2)*(1+tanh(rs[i]*(t-ts[i])/2))
    }
    return(ps[1] + soma)
}

# cálculo do valor estimado da série 
modelo <- function(cs, as, qs, bs, gs, rs, t) {
  c <- calcula_ps(cs, t, rs)
  a <- calcula_ps(as, t, rs)
  q <- calcula_ps(qs, t, rs)
  b <- calcula_ps(bs, t, rs)
  g <- calcula_ps(gs, t, rs)
  return((c*t^a)/((1+b*(q-1)*t^g)^(1/(q-1))))
}

# cálculo da norma l2 dos erros 
residuos_f <- function(args) {
  cs <- args[1:(N)]
  as <- args[(N+1):(2*N)]
  qs <- args[(2*N + 1): (3*N)]
  bs <- args[(3*N+1):(4*N)]
  gs <- args[(4*N+1):(5*N)]
  rs <- args[(5*N+1):length(args)]
  sse <- sqrt(sum((serie_diferenciada - modelo(cs, as, qs, bs, gs, rs, dias))^2))
  return(sse)
}

# restrições dos parâmetros
restricoes <- function(args) {
  a <- args[2*N]
  q <- args[(2*N + 1): (3*N)]
  g <- args[(5*N)]
  return(g - (q - 1)*(a + 1))
}

# Ajuste
opt <- cobyla(x0 = c(rep(1e-3, N),
              rep(4, N),
              rep(1.4, N),
              rep(1e-5, N),
              rep(3, N),
              rep(0.1, N-1)
              ),
       fn=residuos_f,
       lower=c(rep(0, 2*N), rep(1, N), rep(0, 2*N+(N-1))),
       hin = restricoes,
       control = list(xtol_rel = 1e-4, maxeval = 5e5)
)
opt
args <- opt$par
cs <- args[1:(N)]
as <- args[(N+1):(2*N)]
qs <- args[(2*N + 1): (3*N)]
bs <- args[(3*N+1):(4*N)]
gs <- args[(4*N+1):(5*N)]
rs <- args[(5*N+1):length(args)]
# Gráfico do ajuste do modelo
plot(x=dias, y=serie_diferenciada, pch = 20)
curve(modelo(cs, as, qs, bs, gs, rs, x), add=T, col='red', lwd=5)

