source('./tendencia.R')
source('./src/services/carregar_dados.R')
library(drc)

locais <- read.csv('dados/estados_cidades.csv')
series <- c('deaths') # c("confirmed","deaths","vaccines")
estados <- sort(unique(locais$estados))
resultados <- tibble(
  estado="",
  cidade="",
  serie="",
  parametros=list(),
  eqm=Inf,
  epam=Inf,
  elqm=Inf
)
for (estado in estados) {
  cidades <- sort(unique(locais[locais$estados == estado,]$cidades))
  for (cidade in cidades) {
    dados <- read.csv(paste('./dados/cidades/dados', estado, cidade,'.csv'))
    for (serie in series) {
      cat(estado, cidade, serie, '\n')
      dados <- corrige(dados, serie)
      y <- dados[[serie]]
      y <- y[y != 0]
      n <- length(y)
      resultado <- estima_tendencia(y, params=T)
      fit <- resultado$tendencia
      params <- list(resultado$params)
      eqm <-  sum((y - fit)^2) / n
      epam <- sum(abs((y-fit)/y)) / n
      elqm <- sum((log(y) - log(fit))^2) / n
      resultados <- resultados %>% 
        add_row(
          estado=estado,
          cidade=cidade,
          serie=serie,
          parametros=params,
          eqm=eqm,
          epam=epam,
          elqm=elqm
          )
    }
  }
}
