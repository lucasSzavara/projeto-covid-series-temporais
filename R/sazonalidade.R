library(fpp3)
library(dplyr)


estima_sazonalidade <- function(serie, datas) {
  # Pega uma série sem tendência, e modela sua sazonalidade. Para os dados 
  # de covid, temos um melhor resultado ao ajustar a série diferenciada dos dados
  # menos a tendencia ajustada (diff(serie-tendencia))
  
  df <- data.frame(serie, data=datas)
  modelo_sazonalidade <- model(
    df %>% as_tsibble(),
    TSLM(serie ~ season())
  ) 
  sazonalidade_ajustada <- modelo_sazonalidade %>% 
    fitted() %>% 
    group_by(.model) %>% 
    mutate(value = df$serie) %>% 
    mutate(res = value-.fitted)
  return(sazonalidade_ajustada$.fitted)
}

# 
# G0 = 
#   sazonalidade_ajustada %>% 
#   ggplot() +
#   geom_line(aes(x=date, y=value)) +
#   geom_line(aes(x=date, y=.fitted), colour='blue') +
#   facet_wrap(~.model, ncol=3) +
#   labs(
#     x = 'Tempo',
#     y = 'Casos confirmados', 
#     title = 'Sem sazonalidade',
#     colour = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     strip.background = element_rect(fill='lightgray', colour='lightgray')
#   ); G0
# 
# 
# as.data.frame(cbind(ajuste=sazonalidade_ajustada$.fitted + c(0, diff(data$tendencia)), serie=c(0, diff(data$confirmed)), t=1:length(data$confirmed))) %>%
#   ggplot() +
#   geom_point(aes(x=t, y=serie)) +
#   geom_line(aes(x=t, y=ajuste), color='red', size=1)
# 
# 
# 
# as.data.frame(cbind(ajuste=data$tendencia, serie=data$confirmed, t=1:length(data$confirmed))) %>%
#   ggplot() +
#   geom_point(aes(x=t, y=serie)) +
#   geom_line(aes(x=t, y=ajuste), color='red', size=1)
# 
# as.data.frame(cbind(ajuste=cumsum(sazonalidade_ajustada$.fitted) + data$tendencia, serie=data$confirmed, t=1:length(data$confirmed))) %>%
#   ggplot() +
#   geom_point(aes(x=t, y=serie)) +
#   geom_line(aes(x=t, y=ajuste), color='red', size=1)
