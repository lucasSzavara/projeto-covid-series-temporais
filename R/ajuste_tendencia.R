library(nloptr)
library(COVID19)

df <- covid19(country = c('Brazil'), level=1, verbose=F, end = '2022-03-03')
novos_casos <- c(1, diff(df$deaths))
dias <- as.numeric(df$date - df[1, 2])

ts <- as.numeric(as.Date(c('2020-11-24', '2021-12-27')) - df[1, 2])


calcula_ps <- function(ps, t, rs) {
    soma <- 0
    for(i in 1:(length(ps) - 1)) {
      soma <- soma + ((ps[i+1]-ps[i])/2)*(1+tanh(rs[i]*(t-ts[i])/2))
    }
    return(ps[1] + soma)
}
  
modelo_casos <- function(c1, c2, c3, a1, a2, a3, q1, q2, q3, b1, b2, b3, g1, g2, g3, r1, r2, t) {
  rs <- c(r1, r2)
  cs <- c(c1, c2, c3)
  as <- c(a1, a2, a3)
  qs <- c(q1, q2, q3)
  bs <- c(b1, b2, b3)
  gs <- c(g1, g2, g3)
  c <- calcula_ps(cs, t, rs)
  a <- calcula_ps(as, t, rs)
  q <- calcula_ps(qs, t, rs)
  b <- calcula_ps(bs, t, rs)
  g <- calcula_ps(gs, t, rs)
  return((c*t^a)/((1+b*(q-1)*t^g)^(1/(q-1))))
}

residuos_f <- function(args) {
  c1 <- args[1]
  c2 <- args[2]
  c3 <- args[3]
  a1 <- args[4]
  a2 <- args[5]
  a3 <- args[6]
  q1 <- args[7]
  q2 <- args[8]
  q3 <- args[9]
  b1 <- args[10]
  b2 <- args[11]
  b3 <- args[12]
  g1 <- args[13]
  g2 <- args[14]
  g3 <- args[15]
  r1 <- args[16]
  r2 <- args[17]
  sse <- sum((novos_casos - modelo_casos(c1, c2, c3, a1, a2, a3, q1, q2, q3, b1, b2, b3, g1, g2, g3, r1, r2, dias))^2)
  return(sse)
}


restricoes <- function(args) {
  a3 <- args[6]
  q3 <- args[9]
  g3 <- args[15]
  return(g3 - (q3 - 1)*(a3 + 1))
}

opt <- cobyla(x0 = c(rep(1e-3, 3),
              rep(4, 3),
              rep(1.4, 3),
              rep(1e-5, 3),
              rep(3, 3),
              rep(0.1, 2)
              ),
       fn=residuos_f,
       lower=c(rep(0, 6), rep(1, 3), rep(0, 8)),
       hin = restricoes,
       control = list(xtol_rel = 1e-12, maxeval = 1e7)
)
opt
args <- opt$par
c1 <- args[1]
c2 <- args[2]
c3 <- args[3]
a1 <- args[4]
a2 <- args[5]
a3 <- args[6]
q1 <- args[7]
q2 <- args[8]
q3 <- args[9]
b1 <- args[10]
b2 <- args[11]
b3 <- args[12]
g1 <- args[13]
g2 <- args[14]
g3 <- args[15]
r1 <- args[16]
r2 <- args[17]
plot(x=dias, y=novos_casos, pch = 20)
curve(modelo_casos(c1, c2, c3, a1, a2, a3, q1, q2, q3, b1, b2, b3, g1, g2, g3, r1, r2, x), add=T, col='red', lwd=5)

