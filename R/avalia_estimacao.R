source('./tendencia.R')
source('./sazonalidade.R')
source('./src/services/carregar_dados.R')
library(drc)
library('COVID19')
library(ggplot2)
library(zoo)

#------------------------------------------------------------

# Salvar dados de cidades do Brasil em arquivos separados
x <- covid19(country=c('Brazil'), level=3, verbose=F, vintage = "2023-09-30")

locais <- read.csv('dados/estados_cidades.csv')
series <- c('deaths') # c("confirmed","deaths","vaccines")
estados <- sort(unique(locais$estados))
resultados <- tibble(
  estado="",
  cidade="",
  serie="",
  parametros=list(),
  ajuste=list(),
  eqm=Inf,
  epam=Inf,
  elqm=Inf,
  eqm_tendencia=Inf,
  epam_tendencia=Inf,
  elqm_tendencia=Inf,
  chutes=list()
)
erros <- c()
w <- 90
for (estado in estados) {
  cidades <- sort(unique(locais[locais$estados == estado,]$cidades))
  cat('Estado', estado, '\n Nº de cidades:', length(cidades))
  i <- 1
  for (serie in series) {
    for (cidade in cidades) {
      dados <- subset(x, administrative_area_level_2 == estado & administrative_area_level_3 == cidade)
      tryCatch({
          if (i %% 50 == 0) {
              cat(i)
          }
          i <- i + 1
          # serie <- 'deaths'
          # estado <- 'Paraná'
          # cidade <- 'Campo Mourão'
          # dados <- subset(x, administrative_area_level_2 == estado & administrative_area_level_3 == cidade)
          dados <- corrige(dados, serie)
          y <- dados[[serie]]
          n <- length(y)
          # source('./tendencia.R')
          resultado <- estima_tendencia(y,
                                        params=T, 
                                        df=ifelse(sum(
                                          dados$population > 500000
                                          ) > 0, 
                                          yes = 50, 
                                          no=40))
          fit_tend <- resultado$tendencia
          params_tend <- list(resultado$params)
          chutes <- list(resultado$chutes)
          var_estabilizada <- padroniza_variancia(y-fit_tend, width=w, invertible=T)
          y_ <- var_estabilizada$serie
          sd <- var_estabilizada$sd
          saz <- estima_sazonalidade(y_, dados$date[w:length(dados$date)])
          saz <- (saz %>%
                 fitted())['.fitted']
          fit <- fit_tend[w:length(dados$date)] + saz * sd # * p
          eqm_t <-  sum((y - fit_tend)^2) / n
          epam_t <- sum(abs((y-fit_tend)/y)) / n
          elqm_t <- sum((log(y) - log(fit_tend))^2) / n
          eqm <-  sum((y[w:length(dados$date)] - fit)^2) / n
          epam <- sum(abs((y[w:length(dados$date)]-fit)/y)) / n
          elqm <- sum((log(y[w:length(dados$date)]) - log(fit))^2) / n
          resultados <- resultados %>% 
            add_row(
              estado=estado,
              cidade=cidade,
              serie='deaths',
              parametros=params_tend,
              eqm_tendencia=eqm_t,
              epam_tendencia=epam_t,
              elqm_tendencia=elqm_t,
              eqm=eqm,
              epam=epam,
              elqm=elqm,
              ajuste=as.list(fit),
              chutes=chutes
              )
        }, error=function(err) {
          print(paste("MY_ERROR:  ",err))
          cat(estado, cidade, serie, '\n')
          erros <- c(erros, paste(estado, cidade, serie))
        }
      )
    }
  }
}

tamanhos <- c(0, 1e4, 5e4, 1e5, 1e6, Inf)
nomes_tamanhos <- c(
  'muito_pequenas',
  'pequenas',
  'medias',
  'grandes',
  'muito_grandes'
)
cidades_faltantes <- tibble(
  estado="",
  cidade="",
)
for (uf in unique(resultados$estado)) {
  cidades <- sort(unique(resultados[resultados$estado == uf,]$cidade))
  cat('\nEstado', uf, '\n Nº de cidades:', length(cidades), '\n')
  cidades_tamanho <- list(
    muito_pequenas=c(),
    pequenas=c(),
    medias=c(),
    grandes=c(),
    muito_grandes=c()
  )
  for (cidade in cidades) {
    pop <- mean(subset(x, 
                       administrative_area_level_2 == uf & administrative_area_level_3 == cidade)['population'][[1]])
    for(j in 2:(length(tamanhos))) {
      if(tamanhos[j - 1] <= pop && pop < tamanhos[j]){
        cidades_tamanho[[j - 1]] <- append(cidades_tamanho[[j - 1]], cidade)
      }
    }
  }
  i <- 1
  for(j in 1:length(nomes_tamanhos)) {
    pdf(paste('./graficos/', uf, '/', nomes_tamanhos[j], '.pdf', sep=''))
    for(cid in cidades_tamanho[[j]]) {
      if (i %% 50 == 0) {
        cat(i, '\n')
      }
      if (dim(subset(resultados, estado == uf & cidade == cid))[1] == 0) {
        cidades_faltantes <- cidades_faltantes %>% add_row(
          estado=uf,
          cidade=cid,
        )
        next
      }
      i <- i + 1
      dados <- subset(x, administrative_area_level_2 == uf & administrative_area_level_3 == cid)
      # dados <- dados[dados[['deaths']] != 0,]
      dados <- corrige(dados, 'deaths')
      serie <- dados$deaths[w:length(dados$deaths)]
      sub_resultados <- subset(resultados, estado==uf & cidade==cid)
      params <- sub_resultados$parametros[[1]]
      if (length(dim(sub_resultados$chutes[[1]])) == 0 ) {
        chute_inicial <- sub_resultados$chutes[[1]]
      } else {
        chute_inicial <- sub_resultados$chutes[[1]][1,]
      }
      ajuste_inicial <- calcula_tend(chute_inicial, w:(length(serie)+w-1))$n
      ajuste <- sub_resultados$ajuste$.fitted
      g1 <- as.data.frame(cbind(ajuste=ajuste, ajuste_inicial=ajuste_inicial, serie=serie, t=w:(length(serie)+w-1))) %>%
        ggplot() +
        geom_line(aes(x=t, y=serie, color='Real'), linewidth=1) +
        geom_line(aes(x=t, y=ajuste, color='Ajuste'), linewidth=0.5) +
        geom_line(aes(x=t, y=ajuste_inicial, color='Ajuste Inicial'), linewidth=0.5) +
        ggtitle(paste('Gráfico acumulado para a cidade:', cid)) +
        ylab('Mortes por COVID-19') +
        annotate('text', label=paste('Valores ajustados: \n', paste(names(params), round(params, 2), sep = ":", collapse = "\n")), x=length(serie) * 0.8, y=max(serie) * 0.5) +
        annotate('text', label=paste('Chutes Iniciais: \n', paste(names(chute_inicial), round(chute_inicial, 2), sep = ":", collapse = "\n")), x=length(serie) * 0.15, y=max(serie) * 0.7)
      g2 <- as.data.frame(cbind(ajuste=diff(ajuste), ajuste_inicial=diff(ajuste_inicial), serie=diff(serie), t=(w+1):(length(serie)+w-1))) %>%
        ggplot() +
        geom_line(aes(x=t, y=serie, color='Real')) +
        geom_line(aes(x=t, y=ajuste, color='Ajuste'), linewidth=0.5) +
        geom_line(aes(x=t, y=ajuste_inicial, color='Ajuste Inicial'), linewidth=0.5) +
        ggtitle(paste('Gráfico diário para a cidade:', cid)) +
        ylab('Mortes por COVID-19')
      print(g1)
      print(g2)
    }
    dev.off()
  }
}

pdf(paste('./graficos/', uf, '/', 'teste_curitiba', '.pdf', sep=''))
uf <- 'Paraná'
cid <- 'Curitiba'
dados <- subset(x, administrative_area_level_2 == uf & administrative_area_level_3 == cid)
dados <- dados[dados[['deaths']],]
dados <- corrige(dados, 'deaths')
serie <- dados$deaths[w:length(dados$deaths)]
sub_resultados <- subset(resultados, estado==uf & cidade==cid)
params <- sub_resultados$parametros[[1]]
if (length(dim(sub_resultados$chutes[[1]])) == 0 ) {
  chute_inicial <- sub_resultados$chutes[[1]]
} else {
  chute_inicial <- sub_resultados$chutes[[1]][1,]
}
ajuste_inicial <- calcula_tend(chute_inicial, w:(length(serie)+w-1))$n
ajuste <- sub_resultados$ajuste$.fitted
g1 <- as.data.frame(cbind(ajuste=ajuste, ajuste_inicial=ajuste_inicial, serie=serie, t=w:(length(serie)+w-1))) %>%
  ggplot() +
  geom_line(aes(x=t, y=serie, color='Real'), linewidth=1) +
  geom_line(aes(x=t, y=ajuste, color='Ajuste'), linewidth=0.5) +
  geom_line(aes(x=t, y=ajuste_inicial, color='Ajuste Inicial'), linewidth=0.5) +
  ggtitle(paste('Gráfico acumulado para a cidade:', cid)) +
  ylab('Mortes por COVID-19') +
  annotate('text', label=paste('Valores ajustados: \n', paste(names(params), round(params, 2), sep = ":", collapse = "\n")), x=length(serie) * 0.8, y=max(serie) * 0.5) +
  annotate('text', label=paste('Chutes Iniciais: \n', paste(names(chute_inicial), round(chute_inicial, 2), sep = ":", collapse = "\n")), x=length(serie) * 0.15, y=max(serie) * 0.7)
g2 <- as.data.frame(cbind(ajuste=diff(ajuste), ajuste_inicial=diff(ajuste_inicial), serie=diff(serie), t=(w+1):(length(serie)+w-1))) %>%
  ggplot() +
  geom_line(aes(x=t, y=serie, color='Real')) +
  geom_line(aes(x=t, y=ajuste, color='Ajuste'), linewidth=0.5) +
  geom_line(aes(x=t, y=ajuste_inicial, color='Ajuste Inicial'), linewidth=0.5) +
  ggtitle(paste('Gráfico diário para a cidade:', cid)) +
  ylab('Mortes por COVID-19')
print(g1)
print(g2)
dev.off()
