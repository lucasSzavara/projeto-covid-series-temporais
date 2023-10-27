# Carregar pacote
library('COVID19')

#------------------------------------------------------------

# Salvar dados de cidades do Brasil em arquivos separados
x <- covid19(country=c('Brazil'), level=3, verbose=F, vintage = "2023-09-30")
cidades <- c()
estados <- c()
for(uf in unique(x$administrative_area_level_2)){
  cat('\nComeçando estado: ', uf)
  for(cidade in unique(x[x$administrative_area_level_2 == uf,]$administrative_area_level_3)) {
    cat('\nComeçando cidade: ', cidade)
    cidades <- c(cidades, cidade)
    estados <- c(estados, uf)
    write.csv(x[x$administrative_area_level_2 == uf & x$administrative_area_level_3 == cidade,], paste('./dados/cidades/dados', uf, cidade,'.csv'), row.names = F)
  }
}

# Salvar o nome de todos estados e cidades
aux <- data.frame(estados, cidades)
write.csv(aux[order(aux$estados, aux$cidades), ], './dados/auxiliar/estados_cidades.csv', row.names = F)

#------------------------------------------------------------

# Salvar dados de estados do Brasil em arquivos separados
x <- covid19(country=c('Brazil'), level=2, verbose=F, vintage = "2023-09-30")
for(uf in unique(x$administrative_area_level_2)){
  cat('\nComeçando estado: ', uf)
  write.csv(x[x$administrative_area_level_2 == uf,], paste('./dados/estados/dados', uf,'.csv'), row.names = F)
  }

#------------------------------------------------------------

#  Salvar dados do Brasil
z <- covid19(country=c('Brazil'), level=1, verbose=F, vintage = "2023-09-30")
write.csv(z, './dados/pais/dados_pais.csv', row.names = F)
