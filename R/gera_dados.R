library('COVID19')

x <- covid19(country=c('Brazil'), level=3)

for(uf in unique(x$administrative_area_level_2)){
  cat('\nComeçando estado: ', uf)
  for(cidade in unique(x[x$administrative_area_level_2 == uf,]$administrative_area_level_3)) {
    cat('\nComeçando cidade: ', cidade)
    write.csv(x[x$administrative_area_level_2 == uf & x$administrative_area_level_3 == cidade,], paste('~/Documentos/estatistica-ciencia-dados/semestre6/series_temporais/projeto/R/dados/dados', uf, cidade,'.csv'), row.names = F)
  }
}
