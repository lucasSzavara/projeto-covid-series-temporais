library('COVID19')

x <- covid19(country=c('Brazil'), level=3)
cidades <- c()
estados <- c()
for(uf in unique(x$administrative_area_level_2)){
  cat('\nComeçando estado: ', uf)
  for(cidade in unique(x[x$administrative_area_level_2 == uf,]$administrative_area_level_3)) {
    cat('\nComeçando cidade: ', cidade)
    cidades <- c(cidades, cidade)
    estados <- c(estados, uf)
    write.csv(x[x$administrative_area_level_2 == uf & x$administrative_area_level_3 == cidade,], paste('./dados/dados', uf, cidade,'.csv'), row.names = F)
  }
}
aux <- data.frame(estados, cidades)
write.csv(aux[order(aux$estados, aux$cidades), ], './estados_cidades.csv', row.names = F)
