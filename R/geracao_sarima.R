library(astsa)
library(forecast)

set.seed(42)
sample <- sarima.sim(
  ma = c(-0.75),
  sma = c(-0.75),
  S = 7,
  n = 100000
)

ggAcf(sample)
ggPacf(sample)


set.seed(42)
sample <- sarima.sim(
  ma = c(-0.75),
  sma = c(-0.75),
  ar = c(-0.5),
  sar = c(-0.5),
  S = 7,
  n = 100000
)

ggAcf(sample)
ggPacf(sample)
