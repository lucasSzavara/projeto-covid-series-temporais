library(dplyr)
library(drc)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggeasy)



########## Gráfico da interpretação dos parâmetros ##########

calcula_y <- function(b1, d1, e1, b2, d2, e2, b3, d3, e3, t) {
  return (d1 / (1 + exp(b1 * (log(t) - log(e1)))) + d2 / (1 + exp(b2 * (log(t) - log(e2)))) + d3 / (1 + exp(b3 * (log(t) - log(e3)))))
}

t <- seq(1:2000)
b1 <- -4
d1 <- 100
e1 <- 150
b2 <- -12
d2 <- 150
e2 <- 400
b3 <- -8
d3 <- 50
e3 <- 1100

y <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e3,t)
p <- data.frame(t=t, y=y)

title <- expression(paste("Parameters: ",
                          b[1] == -4, " | ",
                          d[1] == 100, " | ",
                          e[1] == 150, " | ",
                          b[2] == -12, " | ",
                          d[2] == 150, " | ",
                          e[2] == 400, " | ",
                          b[3] == -8, " | ",
                          d[3] == 50, " | ",
                          e[3] == 1100))

#y=ax+b
a_1 <- p$y[which(p$y > d1/2)[1]] - p$y[which(p$y > d1/2)[1] - 1]
b_1 <- (p$y[which(p$y > d1/2)[1]] + p$y[which(p$y > d1/2)[1] - 1])/2 - a1*(p$t[which(p$y > d1/2)[1]] - 0.5)
a_2 <- p$y[which(p$y > d1+d2/2)[1]] - p$y[which(p$y > d1+d2/2)[1] - 1]
b_2 <- (p$y[which(p$y > d1+d2/2)[1]] + p$y[which(p$y > d1+d2/2)[1] - 1])/2 - a2*(p$t[which(p$y > d1+d2/2)[1]] - 0.5)
a_3 <- p$y[which(p$y > d1+d2+d3/2)[1]] - p$y[which(p$y > d1+d2+d3/2)[1] - 1]
b_3 <- (p$y[which(p$y > d1+d2+d3/2)[1]] + p$y[which(p$y > d1+d2+d3/2)[1] - 1])/2 - a3*(p$t[which(p$y > d1+d2+d3/2)[1]] - 0.5)

fig_param <- ggplot(p,aes(x=t))+
  geom_line(aes(y = y), linewidth = 1) +
  #linha horizontal para d1
  geom_segment(aes(x = 100, xend = 440, y = d1, yend = d1), linetype = "dotted", linewidth = 0.8) +
  #linha horizontal para d1+d2
  geom_segment(aes(x = 260, xend = 940, y = d1+d2, yend = d1+d2), linetype = "dotted", linewidth = 0.8) +
  #linha horizontal para d1+d2+d3
  geom_segment(aes(x = 1000, xend = 2000, y = d1+d2+d3, yend = d1+d2+d3), linetype = "dotted", linewidth = 0.8) +
  #linha diagonal para b1
  geom_segment(aes(x = 30, xend = 250, y = a_1*30+b_1, yend = a_1*250+b_1), linetype = "dotted", linewidth = 0.8) +
  #linha diagonal para b2
  geom_segment(aes(x = 300, xend = 500, y = a_2*300+b_2, yend = a_2*500+b_2), linetype = "dotted", linewidth = 0.8) +
  #linha diagonal para b3
  geom_segment(aes(x = 650, xend = 1500, y = a_3*650+b_3, yend = a_3*1500+b_3), linetype = "dotted", linewidth = 0.8) +
  #linha vertical para e1
  geom_segment(aes(x = 150, xend = 150, y = 15, yend = 85), linetype = "dotted", linewidth = 0.8) +
  #linha vertical para e2
  geom_segment(aes(x = 402, xend = 402, y = 130, yend = 220), linetype = "dotted", linewidth = 0.8) +
  #linha vertical para e3
  geom_segment(aes(x = 1100.5, xend = 1100.5, y = 245, yend = 305), linetype = "dotted", linewidth = 0.8) +
  annotate(geom = "text", label = expression("d"[1]), x = 75, y = 100, size = 6.5) +
  annotate(geom = "text", label = expression("d"[1]+"d"[2]), x = 187.5, y = 250, size = 6.5) +
  annotate(geom = "text", label = expression("d"[1]+"d"[2]+"d"[3]), x = 885, y = 300, size = 6.5) +
  annotate(geom = "text", label = expression("b"[1]), x = 30, y = -45, size = 6.5) +
  annotate(geom = "text", label = expression("b"[2]), x = 300, y = 45, size = 6.5) +
  annotate(geom = "text", label = expression("b"[3]), x = 650, y = 225, size = 6.5) +
  annotate(geom = "text", label = expression("e"[1]), x = 155, y = 2.5, size = 6.5) +
  annotate(geom = "text", label = expression("e"[2]), x = 415, y = 120, size = 6.5) +
  annotate(geom = "text", label = expression("e"[3]), x = 1105, y = 235, size = 6.5) +
  labs(title = title, x = "Days", y = "Cumulative deaths") +
  theme_minimal() +
  easy_text_size(c("axis.title.x", "axis.title.y"), 13.5) +
  easy_center_title(); fig_param


########## Grade com 9 gráficos com variações dos parâmetros (séries acumuladas) ##########

t <- seq(1:1600)

cols <- c("#fdecea","#fcd9d6","#fbc6c2","#fab3ae","#f9a19a",
          "#f88e86","#f77b72","#f6685e","#f5554a","#f44336",
          "#db3c30","#c3352b","#aa2e25","#922820","#7a211b",
          "#611a15","#491410","#300d0a","#180605","#000000")

# cols <- c("#eceafd","#d9d6fc","#c6c2fb","#b3aefa","#a19af9",
#           "#8e86f8","#7b72f7","#685ef6","#554af5","#4336f4",
#           "#3c30db","#352bc3","#2e25aa","#282092","#211b7a",
#           "#1a1561","#141049","#0d0a30","#060518","#000000")


#####Três ondas / variando b da primeira onda#####

b10 <- -0.2
b11 <- -0.5
b12 <- -1
b13 <- -1.5
b14 <- -2
b15 <- -3
b16 <- -4
b17 <- -6
b18 <- -8
b19 <- -12

y0 <- calcula_y(b10,d1,e1,b2,d2,e2,b3,d3,e3,t)
y1 <- calcula_y(b11,d1,e1,b2,d2,e2,b3,d3,e3,t)
y2 <- calcula_y(b12,d1,e1,b2,d2,e2,b3,d3,e3,t)
y3 <- calcula_y(b13,d1,e1,b2,d2,e2,b3,d3,e3,t)
y4 <- calcula_y(b14,d1,e1,b2,d2,e2,b3,d3,e3,t)
y5 <- calcula_y(b15,d1,e1,b2,d2,e2,b3,d3,e3,t)
y6 <- calcula_y(b16,d1,e1,b2,d2,e2,b3,d3,e3,t)
y7 <- calcula_y(b17,d1,e1,b2,d2,e2,b3,d3,e3,t)
y8 <- calcula_y(b18,d1,e1,b2,d2,e2,b3,d3,e3,t)
y9 <- calcula_y(b19,d1,e1,b2,d2,e2,b3,d3,e3,t)

p_b1 <- data.frame(t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9)
expression(paste("b"^"*"))
title_b1 <- expression(paste(b[1] == b^"*"," | ",b[2] == -12," | ",b[3] == -8))
lw <- 0.8

fig_b1 <- ggplot(p_b1,aes(x=t))+
  geom_line(aes(y = y0, color = "z = -0.2"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = -0.5"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = -1"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = -1.5"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = -2"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = -3"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = -4"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = -6"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = -8"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = -12"), linewidth = lw) +
  scale_color_manual(values = c("z = -0.2" = cols[1], "z = -0.5" = cols[3],
                                "z = -1" = cols[5], "z = -1.5" = cols[7],
                                "z = -2" = cols[9], "z = -3" = cols[11],
                                "z = -4" = cols[13], "z = -6" = cols[14],
                                "z = -8" = cols[15], "z = -12" = cols[19])) +
  labs(colour = expression('b'[1]), title = title_b1, x = "Days", y = "Cumulative deaths") +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_b1


#####Três ondas / variando b da segunda onda#####
b20 <- -0.2
b21 <- -0.5
b22 <- -1
b23 <- -1.5
b24 <- -2
b25 <- -3
b26 <- -4
b27 <- -6
b28 <- -8
b29 <- -12

y0 <- calcula_y(b1,d1,e1,b20,d2,e2,b3,d3,e3,t)
y1 <- calcula_y(b1,d1,e1,b21,d2,e2,b3,d3,e3,t)
y2 <- calcula_y(b1,d1,e1,b22,d2,e2,b3,d3,e3,t)
y3 <- calcula_y(b1,d1,e1,b23,d2,e2,b3,d3,e3,t)
y4 <- calcula_y(b1,d1,e1,b24,d2,e2,b3,d3,e3,t)
y5 <- calcula_y(b1,d1,e1,b25,d2,e2,b3,d3,e3,t)
y6 <- calcula_y(b1,d1,e1,b26,d2,e2,b3,d3,e3,t)
y7 <- calcula_y(b1,d1,e1,b27,d2,e2,b3,d3,e3,t)
y8 <- calcula_y(b1,d1,e1,b28,d2,e2,b3,d3,e3,t)
y9 <- calcula_y(b1,d1,e1,b29,d2,e2,b3,d3,e3,t)

p_b2 <- data.frame(t=t, t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9)

title_b2 <- expression(paste(b[1] == -4," | ",b[2] == b^"*"," | ",b[3] == -8))

fig_b2 <- ggplot(p_b2,aes(x=t))+
  geom_line(aes(y = y0, color = "z = -0.2"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = -0.5"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = -1"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = -1.5"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = -2"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = -3"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = -4"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = -6"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = -8"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = -12"), linewidth = lw) +
  scale_color_manual(values = c("z = -0.2" = cols[1], "z = -0.5" = cols[3],
                                "z = -1" = cols[5], "z = -1.5" = cols[7],
                                "z = -2" = cols[9], "z = -3" = cols[11],
                                "z = -4" = cols[13], "z = -6" = cols[14],
                                "z = -8" = cols[15], "z = -12" = cols[19])) +
  labs(colour = expression('b'[2]), title = title_b2, x = "Days", y = NULL) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_b2


#####Três ondas / variando b da terceira onda#####
b30 <- -0.2
b31 <- -0.5
b32 <- -1
b33 <- -1.5
b34 <- -2
b35 <- -3
b36 <- -4
b37 <- -6
b38 <- -8
b39 <- -12

y0 <- calcula_y(b1,d1,e1,b2,d2,e2,b30,d3,e3,t)
y1 <- calcula_y(b1,d1,e1,b2,d2,e2,b31,d3,e3,t)
y2 <- calcula_y(b1,d1,e1,b2,d2,e2,b32,d3,e3,t)
y3 <- calcula_y(b1,d1,e1,b2,d2,e2,b33,d3,e3,t)
y4 <- calcula_y(b1,d1,e1,b2,d2,e2,b34,d3,e3,t)
y5 <- calcula_y(b1,d1,e1,b2,d2,e2,b35,d3,e3,t)
y6 <- calcula_y(b1,d1,e1,b2,d2,e2,b36,d3,e3,t)
y7 <- calcula_y(b1,d1,e1,b2,d2,e2,b37,d3,e3,t)
y8 <- calcula_y(b1,d1,e1,b2,d2,e2,b38,d3,e3,t)
y9 <- calcula_y(b1,d1,e1,b2,d2,e2,b39,d3,e3,t)

p_b3 <- data.frame(t=t, t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9)

title_b3 <- expression(paste(b[1] == -4," | ",b[2] == -12," | ",b[3] == b^"*"))

fig_b3 <- ggplot(p_b3,aes(x=t))+
  geom_line(aes(y = y0, color = "z = -0.2"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = -0.5"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = -1"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = -1.5"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = -2"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = -3"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = -4"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = -6"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = -8"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = -12"), linewidth = lw) +
  scale_color_manual(values = c("z = -0.2" = cols[1], "z = -0.5" = cols[3],
                                "z = -1" = cols[5], "z = -1.5" = cols[7],
                                "z = -2" = cols[9], "z = -3" = cols[11],
                                "z = -4" = cols[13], "z = -6" = cols[14],
                                "z = -8" = cols[15], "z = -12" = cols[19])) +
  labs(colour = NULL, title = title_b3, x = "Days", y = NULL) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_b3


#####Três ondas / variando d da primeira onda#####
d10 <- 50
d11 <- 60
d12 <- 70
d13 <- 80
d14 <- 90
d15 <- 100
d16 <- 110
d17 <- 120
d18 <- 130
d19 <- 140
d1a <- 150

y0 <- calcula_y(b1,d10,e1,b2,d2,e2,b3,d3,e3,t)
y1 <- calcula_y(b1,d11,e1,b2,d2,e2,b3,d3,e3,t)
y2 <- calcula_y(b1,d12,e1,b2,d2,e2,b3,d3,e3,t)
y3 <- calcula_y(b1,d13,e1,b2,d2,e2,b3,d3,e3,t)
y4 <- calcula_y(b1,d14,e1,b2,d2,e2,b3,d3,e3,t)
y5 <- calcula_y(b1,d15,e1,b2,d2,e2,b3,d3,e3,t)
y6 <- calcula_y(b1,d16,e1,b2,d2,e2,b3,d3,e3,t)
y7 <- calcula_y(b1,d17,e1,b2,d2,e2,b3,d3,e3,t)
y8 <- calcula_y(b1,d18,e1,b2,d2,e2,b3,d3,e3,t)
y9 <- calcula_y(b1,d19,e1,b2,d2,e2,b3,d3,e3,t)
ya <- calcula_y(b1,d1a,e1,b2,d2,e2,b3,d3,e3,t)

p_d1 <- data.frame(t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9, ya=ya)

title_d1 <- expression(paste(d[1] == d^"*"," | ",d[2] == 150," | ",d[3] == 50))

fig_d1 <- ggplot(p_d1,aes(x=t))+
  geom_line(aes(y = y0, color = "d1 = 50"), linewidth = lw) +
  geom_line(aes(y = y1, color = "d1 = 60"), linewidth = lw) +
  geom_line(aes(y = y2, color = "d1 = 70"), linewidth = lw) +
  geom_line(aes(y = y3, color = "d1 = 80"), linewidth = lw) +
  geom_line(aes(y = y4, color = "d1 = 90"), linewidth = lw) +
  geom_line(aes(y = y5, color = "d1 = 100"), linewidth = lw) +
  geom_line(aes(y = y6, color = "d1 = 110"), linewidth = lw) +
  geom_line(aes(y = y7, color = "d1 = 120"), linewidth = lw) +
  geom_line(aes(y = y8, color = "d1 = 130"), linewidth = lw) +
  geom_line(aes(y = y9, color = "d1 = 140"), linewidth = lw) +
  geom_line(aes(y = ya, color = "d1 = 150"), linewidth = lw) +
  scale_color_manual(values = c("d1 = 50" = cols[1], "d1 = 60" = cols[3],
                                "d1 = 70" = cols[5], "d1 = 80" = cols[7],
                                "d1 = 90" = cols[9], "d1 = 100" = cols[11],
                                "d1 = 110" = cols[12], "d1 = 120" = cols[14],
                                "d1 = 130" = cols[16], "d1 = 140" = cols[18],
                                "d1 = 150" = cols[20])) +
  labs(colour = expression('d'[1]), title = title_d1, x = "Days", y = "Cumulative deaths") +
  coord_cartesian(ylim = c(0 , 400)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_d1


#####Três ondas / variando d da segunda onda#####
d20 <- 50
d21 <- 60
d22 <- 70
d23 <- 80
d24 <- 90
d25 <- 100
d26 <- 110
d27 <- 120
d28 <- 130
d29 <- 140
d2a <- 150

y0 <- calcula_y(b1,d1,e1,b2,d20,e2,b3,d3,e3,t)
y1 <- calcula_y(b1,d1,e1,b2,d21,e2,b3,d3,e3,t)
y2 <- calcula_y(b1,d1,e1,b2,d22,e2,b3,d3,e3,t)
y3 <- calcula_y(b1,d1,e1,b2,d23,e2,b3,d3,e3,t)
y4 <- calcula_y(b1,d1,e1,b2,d24,e2,b3,d3,e3,t)
y5 <- calcula_y(b1,d1,e1,b2,d25,e2,b3,d3,e3,t)
y6 <- calcula_y(b1,d1,e1,b2,d26,e2,b3,d3,e3,t)
y7 <- calcula_y(b1,d1,e1,b2,d27,e2,b3,d3,e3,t)
y8 <- calcula_y(b1,d1,e1,b2,d28,e2,b3,d3,e3,t)
y9 <- calcula_y(b1,d1,e1,b2,d29,e2,b3,d3,e3,t)
ya <- calcula_y(b1,d1,e1,b2,d2a,e2,b3,d3,e3,t)

p_d2 <- data.frame(t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9, ya=ya)

title_d2 <- expression(paste(d[1] == 100," | ",d[2] == d^"*"," | ",d[3] == 50))

fig_d2 <- ggplot(p_d2,aes(x=t))+
  geom_line(aes(y = y0, color = "d2 = 50"), linewidth = lw) +
  geom_line(aes(y = y1, color = "d2 = 60"), linewidth = lw) +
  geom_line(aes(y = y2, color = "d2 = 70"), linewidth = lw) +
  geom_line(aes(y = y3, color = "d2 = 80"), linewidth = lw) +
  geom_line(aes(y = y4, color = "d2 = 90"), linewidth = lw) +
  geom_line(aes(y = y5, color = "d2 = 100"), linewidth = lw) +
  geom_line(aes(y = y6, color = "d2 = 110"), linewidth = lw) +
  geom_line(aes(y = y7, color = "d2 = 120"), linewidth = lw) +
  geom_line(aes(y = y8, color = "d2 = 130"), linewidth = lw) +
  geom_line(aes(y = y9, color = "d2 = 140"), linewidth = lw) +
  geom_line(aes(y = ya, color = "d2 = 150"), linewidth = lw) +
  scale_color_manual(values = c("d2 = 50" = cols[1], "d2 = 60" = cols[3],
                                "d2 = 70" = cols[5], "d2 = 80" = cols[7],
                                "d2 = 90" = cols[9], "d2 = 100" = cols[11],
                                "d2 = 110" = cols[12], "d2 = 120" = cols[14],
                                "d2 = 130" = cols[16], "d2 = 140" = cols[18],
                                "d2 = 150" = cols[20])) +
  labs(colour = expression('d'[2]), title = title_d2, x = "Days", y = NULL) +
  coord_cartesian(ylim = c(0 , 400)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_d2


#####Três ondas / variando d da terceira onda#####
d30 <- 50
d31 <- 60
d32 <- 70
d33 <- 80
d34 <- 90
d35 <- 100
d36 <- 110
d37 <- 120
d38 <- 130
d39 <- 140
d3a <- 150

y0 <- calcula_y(b1,d1,e1,b2,d20,e2,b3,d3,e3,t)
y1 <- calcula_y(b1,d1,e1,b2,d21,e2,b3,d3,e3,t)
y2 <- calcula_y(b1,d1,e1,b2,d22,e2,b3,d3,e3,t)
y3 <- calcula_y(b1,d1,e1,b2,d23,e2,b3,d3,e3,t)
y4 <- calcula_y(b1,d1,e1,b2,d24,e2,b3,d3,e3,t)
y5 <- calcula_y(b1,d1,e1,b2,d25,e2,b3,d3,e3,t)
y6 <- calcula_y(b1,d1,e1,b2,d26,e2,b3,d3,e3,t)
y7 <- calcula_y(b1,d1,e1,b2,d27,e2,b3,d3,e3,t)
y8 <- calcula_y(b1,d1,e1,b2,d28,e2,b3,d3,e3,t)
y9 <- calcula_y(b1,d1,e1,b2,d29,e2,b3,d3,e3,t)
ya <- calcula_y(b1,d1,e1,b2,d2a,e2,b3,d3,e3,t)

p_d3 <- data.frame(t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9, ya=ya)

title_d3 <- expression(paste(d[1] == 100," | ",d[2] == 150," | ",d[3] == d^"*"))

fig_d3 <- ggplot(p_d3,aes(x=t))+
  geom_line(aes(y = y0, color = "z = 50"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = 60"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = 70"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = 80"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = 90"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = 100"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = 110"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = 120"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = 130"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = 140"), linewidth = lw) +
  geom_line(aes(y = ya, color = "z = 150"), linewidth = lw) +
  scale_color_manual(values = c("z = 50" = cols[1], "z = 60" = cols[3],
                                "z = 70" = cols[5], "z = 80" = cols[7],
                                "z = 90" = cols[9], "z = 100" = cols[11],
                                "z = 110" = cols[12], "z = 120" = cols[14],
                                "z = 130" = cols[16], "z = 140" = cols[18],
                                "z = 150" = cols[20])) +
  labs(colour = NULL, title = title_d3, x = "Days", y = NULL) +
  coord_cartesian(ylim = c(0 , 400)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_d3


#####Três ondas / variando e da primeira onda#####
e10 <- 150*exp(-0.5)
e11 <- 150*exp(-0.4)
e12 <- 150*exp(-0.3)
e13 <- 150*exp(-0.2)
e14 <- 150*exp(-0.1)
e15 <- 150
e16 <- 150*exp(0.1)
e17 <- 150*exp(0.2)
e18 <- 150*exp(0.3)
e19 <- 150*exp(0.4)
e1a <- 150*exp(0.5)

y0 <- calcula_y(b1,d1,e10,b2,d2,e2,b3,d3,e3,t)
y1 <- calcula_y(b1,d1,e11,b2,d2,e2,b3,d3,e3,t)
y2 <- calcula_y(b1,d1,e12,b2,d2,e2,b3,d3,e3,t)
y3 <- calcula_y(b1,d1,e13,b2,d2,e2,b3,d3,e3,t)
y4 <- calcula_y(b1,d1,e14,b2,d2,e2,b3,d3,e3,t)
y5 <- calcula_y(b1,d1,e15,b2,d2,e2,b3,d3,e3,t)
y6 <- calcula_y(b1,d1,e16,b2,d2,e2,b3,d3,e3,t)
y7 <- calcula_y(b1,d1,e17,b2,d2,e2,b3,d3,e3,t)
y8 <- calcula_y(b1,d1,e18,b2,d2,e2,b3,d3,e3,t)
y9 <- calcula_y(b1,d1,e19,b2,d2,e2,b3,d3,e3,t)
ya <- calcula_y(b1,d1,e1a,b2,d2,e2,b3,d3,e3,t)

p_e1 <- data.frame(t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9, ya=ya)

title_e1 <- expression(paste(e[1] == 150 %*% exp(e^"*")," | ",e[2] == 400," | ",e[3] == 1100))

fig_e1 <- ggplot(p_e1,aes(x=t))+
  geom_line(aes(y = y0, color = "e1 = 75"), linewidth = lw) +
  geom_line(aes(y = y1, color = "e1 = 90"), linewidth = lw) +
  geom_line(aes(y = y2, color = "e1 = 105"), linewidth = lw) +
  geom_line(aes(y = y3, color = "e1 = 120"), linewidth = lw) +
  geom_line(aes(y = y4, color = "e1 = 135"), linewidth = lw) +
  geom_line(aes(y = y5, color = "e1 = 150"), linewidth = lw) +
  geom_line(aes(y = y6, color = "e1 = 165"), linewidth = lw) +
  geom_line(aes(y = y7, color = "e1 = 180"), linewidth = lw) +
  geom_line(aes(y = y8, color = "e1 = 195"), linewidth = lw) +
  geom_line(aes(y = y9, color = "e1 = 210"), linewidth = lw) +
  geom_line(aes(y = ya, color = "e1 = 225"), linewidth = lw) +
  scale_color_manual(values = c("e1 = 75" = cols[1], "e1 = 90" = cols[3],
                                "e1 = 105" = cols[5], "e1 = 120" = cols[7],
                                "e1 = 135" = cols[9], "e1 = 150" = cols[11],
                                "e1 = 165" = cols[12], "e1 = 180" = cols[14],
                                "e1 = 195" = cols[16], "e1 = 210" = cols[18],
                                "e1 = 225" = cols[20])) +
  labs(colour = expression('e'[1]), title = title_e1, x = "Days", y = "Cumulative deaths") +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_e1


#####Três ondas / variando e da segunda onda#####
e20 <- 400*exp(-0.5)
e21 <- 400*exp(-0.4)
e22 <- 400*exp(-0.3)
e23 <- 400*exp(-0.2)
e24 <- 400*exp(-0.1)
e25 <- 400
e26 <- 400*exp(0.1)
e27 <- 400*exp(0.2)
e28 <- 400*exp(0.3)
e29 <- 400*exp(0.4)
e2a <- 400*exp(0.5)

y0 <- calcula_y(b1,d1,e1,b2,d2,e20,b3,d3,e3,t)
y1 <- calcula_y(b1,d1,e1,b2,d2,e21,b3,d3,e3,t)
y2 <- calcula_y(b1,d1,e1,b2,d2,e22,b3,d3,e3,t)
y3 <- calcula_y(b1,d1,e1,b2,d2,e23,b3,d3,e3,t)
y4 <- calcula_y(b1,d1,e1,b2,d2,e24,b3,d3,e3,t)
y5 <- calcula_y(b1,d1,e1,b2,d2,e25,b3,d3,e3,t)
y6 <- calcula_y(b1,d1,e1,b2,d2,e26,b3,d3,e3,t)
y7 <- calcula_y(b1,d1,e1,b2,d2,e27,b3,d3,e3,t)
y8 <- calcula_y(b1,d1,e1,b2,d2,e28,b3,d3,e3,t)
y9 <- calcula_y(b1,d1,e1,b2,d2,e29,b3,d3,e3,t)
ya <- calcula_y(b1,d1,e1,b2,d2,e2a,b3,d3,e3,t)

p_e2 <- data.frame(t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9, ya=ya)

title_e2 <- expression(paste(e[1] == 150," | ",e[2] == 400 %*% exp(e^"*")," | ",e[3] == 1100))

fig_e2 <- ggplot(p_e2,aes(x=t))+
  geom_line(aes(y = y0, color = "e2 = 5.5"), linewidth = lw) +
  geom_line(aes(y = y1, color = "e2 = 5.6"), linewidth = lw) +
  geom_line(aes(y = y2, color = "e2 = 5.7"), linewidth = lw) +
  geom_line(aes(y = y3, color = "e2 = 5.8"), linewidth = lw) +
  geom_line(aes(y = y4, color = "e2 = 5.9"), linewidth = lw) +
  geom_line(aes(y = y5, color = "e2 = 6"), linewidth = lw) +
  geom_line(aes(y = y6, color = "e2 = 6.1"), linewidth = lw) +
  geom_line(aes(y = y7, color = "e2 = 6.2"), linewidth = lw) +
  geom_line(aes(y = y8, color = "e2 = 6.3"), linewidth = lw) +
  geom_line(aes(y = y9, color = "e2 = 6.4"), linewidth = lw) +
  geom_line(aes(y = ya, color = "e2 = 6.5"), linewidth = lw) +
  scale_color_manual(values = c("e2 = 5.5" = cols[1], "e2 = 5.6" = cols[3],
                                "e2 = 5.7" = cols[5], "e2 = 5.8" = cols[7],
                                "e2 = 5.9" = cols[9], "e2 = 6" = cols[11],
                                "e2 = 6.1" = cols[12], "e2 = 6.2" = cols[14],
                                "e2 = 6.3" = cols[16], "e2 = 6.4" = cols[18],
                                "e2 = 6.5" = cols[20])) +
  labs(title = title_e2, x = "Days", y = NULL) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_e2


#####Três ondas / variando e da terceira onda#####
e30 <- 1100*exp(-0.5)
e31 <- 1100*exp(-0.4)
e32 <- 1100*exp(-0.3)
e33 <- 1100*exp(-0.2)
e34 <- 1100*exp(-0.1)
e35 <- 1100
e36 <- 1100*exp(0.1)
e37 <- 1100*exp(0.2)
e38 <- 1100*exp(0.3)
e39 <- 1100*exp(0.4)
e3a <- 1100*exp(0.5)

y0 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e30,t)
y1 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e31,t)
y2 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e32,t)
y3 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e33,t)
y4 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e34,t)
y5 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e35,t)
y6 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e36,t)
y7 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e37,t)
y8 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e38,t)
y9 <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e39,t)
ya <- calcula_y(b1,d1,e1,b2,d2,e2,b3,d3,e3a,t)

p_e3 <- data.frame(t=t, y0=y0, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8, y9=y9, ya=ya)

title_e3 <- expression(paste(e[1] == 150," | ",e[2] == 400," | ",e[3] == 1100 %*% exp(e^"*")))

fig_e3 <- ggplot(p_e3,aes(x=t))+
  geom_line(aes(y = y0, color = "z = -0.5"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = -0.4"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = -0.3"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = -0.2"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = -0.1"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = 0"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = 0.1"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = 0.2"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = 0.3"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = 0.4"), linewidth = lw) +
  geom_line(aes(y = ya, color = "z = 0.5"), linewidth = lw) +
  scale_color_manual(values = c("z = -0.5" = cols[1], "z = -0.4" = cols[3],
                                "z = -0.3" = cols[5], "z = -0.2" = cols[7],
                                "z = -0.1" = cols[9], "z = 0" = cols[11],
                                "z = 0.1" = cols[12], "z = 0.2" = cols[14],
                                "z = 0.3" = cols[16], "z = 0.4" = cols[18],
                                "z = 0.5" = cols[20])) +
  labs(colour = NULL, title = title_e3, x = "Days", y = NULL) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_e3


g <- rasterGrob(c(cols[20],cols[19],cols[18],cols[17],cols[16],
                  cols[15],cols[14],cols[13],cols[12],cols[11],
                  cols[10],cols[9],cols[8],cols[7],cols[6],
                  cols[5],cols[4],cols[3],cols[2],cols[1]), 
                width=unit(1,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE) 


#####escala para b#####
fig_sc_b <- ggplot() +
  annotation_custom(g, xmin=10, xmax=100, ymin=-0.1, ymax=-12) +   
  scale_x_continuous(expand = c(0, 0), limits = c(-20, 120)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.1, -12), breaks = c(-0.2, -3, -6, -9, -12)) +
  ggtitle(expression(paste("b"^"*"))) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),
    #axis.ticks.y=element_blank(),
    #axis.text.y=element_blank(),
    #axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank()
  ) +
  #theme_void()+
  coord_cartesian(xlim = c(-20, 120), ylim = c(-0.2 , -12))#; fig_sc_b
#annotate(geom = "text", label = "z", size = 5, x = 50, y = 160)


#####escala para d#####
fig_sc_d <- ggplot() +
  annotation_custom(g, xmin=10, xmax=100, ymin=50, ymax=150) +   
  scale_x_continuous(expand = c(0, 0), limits = c(-20, 120)) +
  scale_y_continuous(expand = c(0, 0), limits = c(50, 150)) +
  ggtitle(expression(paste("d"^"*"))) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),
    #axis.ticks.y=element_blank(),
    #axis.text.y=element_blank(),
    #axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank()
  ) +
  #theme_void()+
  coord_cartesian(xlim = c(-20, 120), ylim = c(50 , 150))#; fig_sc_d 
#annotate(geom = "text", label = "z", size = 5, x = 50, y = 160)


#####escala para e#####
fig_sc_e <- ggplot() +
  annotation_custom(g, xmin=10, xmax=100, ymin=-0.5, ymax=0.5) +   
  scale_x_continuous(expand = c(0, 0), limits = c(-20, 120)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 0.5)) +
  ggtitle(expression(paste("e"^"*"))) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),
    #axis.ticks.y=element_blank(),
    #axis.text.y=element_blank(),
    #axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank()
  ) +
  #theme_void()+
  coord_cartesian(xlim = c(-20, 120), ylim = c(-0.5 , 0.5))#; fig_sc_e
#annotate(geom = "text", label = "z", size = 5, x = 50, y = 160)


#####grade com os 9 gráficos#####
G1 <- ggarrange(fig_b1, fig_b2, fig_b3, fig_sc_b, widths = c(2,2,2,0.35), ncol=4, nrow=1)
title_G1 <- expression(paste("Fixed parameters: ",d[1] == 100," | ",e[1] == 150," | ",d[2] == 150," | ",e[2] == 400," | ",d[3] == 50," | ",e[3] == 1100))
title_G1 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
                     textGrob(title_G1, x=0.5, hjust=0.5,
                              gp=gpar(col="black", cex = 1.2)))
#G1 <- grid.arrange(title_G1, G1, heights=c(1,9))
G1 <- ggarrange(title_G1, G1, heights=c(1,9), ncol=1, nrow=2)
#G1 <- annotate_figure(G1, top = text_grob(title_G1, face = "bold", size = 14))

G2 <- ggarrange(fig_d1, fig_d2, fig_d3, fig_sc_d, widths = c(2,2,2,0.35), ncol=4, nrow=1)
title_G2 <- expression(paste("Fixed parameters: ",b[1] == -4," | ",e[1] == 150," | ",b[2] == -12," | ",e[2] == 400," | ",b[3] == -8," | ",e[3] == 1100))
title_G2 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
                     textGrob(title_G2, x=0.5, hjust=0.5,
                              gp=gpar(col="black", cex = 1.2)))
#G2 <- grid.arrange(title_G2, G2, heights=c(1,9))
G2 <- ggarrange(title_G2, G2, heights=c(1,9), ncol=1, nrow=2)
#G2 <- annotate_figure(G2, top = text_grob(title_G2, face = "bold", size = 14))

G3 <- ggarrange(fig_e1, fig_e2, fig_e3, fig_sc_e, widths = c(2,2,2,0.35), ncol=4, nrow=1)
title_G3 <- expression(paste("Fixed parameters: ",b[1] == -4," | ",d[1] == 100," | ",b[2] == -12," | ",d[2] == 150," | ",b[3] == -8," | ",d[3] == 50))
title_G3 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
                     textGrob(title_G3, x=0.5, hjust=0.5,
                              gp=gpar(col="black", cex = 1.2)))
#G3 <- grid.arrange(title_G3, G3, heights=c(1,9))
G3 <- ggarrange(title_G3, G3, heights=c(1,9), ncol=1, nrow=2)
#G3 <- annotate_figure(G3, top = text_grob(title_G3, face = "bold", size = 14))

grid1 <- ggarrange(G1,G2,G3,ncol=1,nrow=3);grid1



########## Grade com 9 gráficos com variações dos parâmetros (séries diárias) ##########

#####Três ondas / variando b da primeira onda#####
p_b1_d <- data.frame(t=t[2:length(t)], y0=diff(p_b1$y0), y1=diff(p_b1$y1), y2=diff(p_b1$y2), y3=diff(p_b1$y3),
                     y4=diff(p_b1$y4), y5=diff(p_b1$y5), y6=diff(p_b1$y6), y7=diff(p_b1$y7), y8=diff(p_b1$y8), y9=diff(p_b1$y9))

expression(paste("b"^"*"))
lw <- 0.8

fig_b1_d <- ggplot(p_b1_d,aes(x=t))+
  geom_line(aes(y = y0, color = "z = -0.2"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = -0.5"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = -1"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = -1.5"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = -2"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = -3"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = -4"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = -6"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = -8"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = -12"), linewidth = lw) +
  scale_color_manual(values = c("z = -0.2" = cols[1], "z = -0.5" = cols[3],
                                "z = -1" = cols[5], "z = -1.5" = cols[7],
                                "z = -2" = cols[9], "z = -3" = cols[11],
                                "z = -4" = cols[13], "z = -6" = cols[14],
                                "z = -8" = cols[15], "z = -12" = cols[19])) +
  labs(colour = expression('b'[1]), title = title_b1, x = "Days", y = "Deaths") +
  coord_cartesian(ylim = c(0 , 2.1)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_b1_d


#####Três ondas / variando b da segunda onda#####
p_b2_d <- data.frame(t=t[2:length(t)], y0=diff(p_b2$y0), y1=diff(p_b2$y1), y2=diff(p_b2$y2), y3=diff(p_b2$y3),
                     y4=diff(p_b2$y4), y5=diff(p_b2$y5), y6=diff(p_b2$y6), y7=diff(p_b2$y7), y8=diff(p_b2$y8), y9=diff(p_b2$y9))

fig_b2_d <- ggplot(p_b2_d,aes(x=t))+
  geom_line(aes(y = y0, color = "z = -0.2"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = -0.5"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = -1"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = -1.5"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = -2"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = -3"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = -4"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = -6"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = -8"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = -12"), linewidth = lw) +
  scale_color_manual(values = c("z = -0.2" = cols[1], "z = -0.5" = cols[3],
                                "z = -1" = cols[5], "z = -1.5" = cols[7],
                                "z = -2" = cols[9], "z = -3" = cols[11],
                                "z = -4" = cols[13], "z = -6" = cols[14],
                                "z = -8" = cols[15], "z = -12" = cols[19])) +
  labs(colour = expression('b'[2]), title = title_b2, x = "Days", y = NULL) +
  coord_cartesian(ylim = c(0 , 2.1)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_b2_d


#####Três ondas / variando b da terceira onda#####
p_b3_d <- data.frame(t=t[2:length(t)], y0=diff(p_b3$y0), y1=diff(p_b3$y1), y2=diff(p_b3$y2), y3=diff(p_b3$y3),
                     y4=diff(p_b3$y4), y5=diff(p_b3$y5), y6=diff(p_b3$y6), y7=diff(p_b3$y7), y8=diff(p_b3$y8), y9=diff(p_b3$y9))

fig_b3_d <- ggplot(p_b3_d,aes(x=t))+
  geom_line(aes(y = y0, color = "z = -0.2"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = -0.5"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = -1"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = -1.5"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = -2"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = -3"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = -4"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = -6"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = -8"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = -12"), linewidth = lw) +
  scale_color_manual(values = c("z = -0.2" = cols[1], "z = -0.5" = cols[3],
                                "z = -1" = cols[5], "z = -1.5" = cols[7],
                                "z = -2" = cols[9], "z = -3" = cols[11],
                                "z = -4" = cols[13], "z = -6" = cols[14],
                                "z = -8" = cols[15], "z = -12" = cols[19])) +
  labs(colour = NULL, title = title_b3, x = "Days", y = NULL) +
  coord_cartesian(ylim = c(0 , 2.1)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_b3_d


#####Três ondas / variando d da primeira onda#####

p_d1_d <- data.frame(t=t[2:length(t)], y0=diff(p_d1$y0), y1=diff(p_d1$y1), y2=diff(p_d1$y2), y3=diff(p_d1$y3),
                     y4=diff(p_d1$y4), y5=diff(p_d1$y5), y6=diff(p_d1$y6), y7=diff(p_d1$y7), y8=diff(p_d1$y8),
                     y9=diff(p_d1$y9), ya=diff(p_d1$ya))

fig_d1_d <- ggplot(p_d1_d,aes(x=t))+
  geom_line(aes(y = y0, color = "d1 = 50"), linewidth = lw) +
  geom_line(aes(y = y1, color = "d1 = 60"), linewidth = lw) +
  geom_line(aes(y = y2, color = "d1 = 70"), linewidth = lw) +
  geom_line(aes(y = y3, color = "d1 = 80"), linewidth = lw) +
  geom_line(aes(y = y4, color = "d1 = 90"), linewidth = lw) +
  geom_line(aes(y = y5, color = "d1 = 100"), linewidth = lw) +
  geom_line(aes(y = y6, color = "d1 = 110"), linewidth = lw) +
  geom_line(aes(y = y7, color = "d1 = 120"), linewidth = lw) +
  geom_line(aes(y = y8, color = "d1 = 130"), linewidth = lw) +
  geom_line(aes(y = y9, color = "d1 = 140"), linewidth = lw) +
  geom_line(aes(y = ya, color = "d1 = 150"), linewidth = lw) +
  scale_color_manual(values = c("d1 = 50" = cols[1], "d1 = 60" = cols[3],
                                "d1 = 70" = cols[5], "d1 = 80" = cols[7],
                                "d1 = 90" = cols[9], "d1 = 100" = cols[11],
                                "d1 = 110" = cols[12], "d1 = 120" = cols[14],
                                "d1 = 130" = cols[16], "d1 = 140" = cols[18],
                                "d1 = 150" = cols[20])) +
  labs(colour = expression('d'[1]), title = title_d1, x = "Days", y = "Deaths") +
  coord_cartesian(ylim = c(0 , 1.2)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_d1_d


#####Três ondas / variando d da segunda onda#####

p_d2_d <- data.frame(t=t[2:length(t)], y0=diff(p_d2$y0), y1=diff(p_d2$y1), y2=diff(p_d2$y2), y3=diff(p_d2$y3),
                     y4=diff(p_d2$y4), y5=diff(p_d2$y5), y6=diff(p_d2$y6), y7=diff(p_d2$y7), y8=diff(p_d2$y8),
                     y9=diff(p_d2$y9), ya=diff(p_d2$ya))

fig_d2_d <- ggplot(p_d2_d,aes(x=t))+
  geom_line(aes(y = y0, color = "d2 = 50"), linewidth = lw) +
  geom_line(aes(y = y1, color = "d2 = 60"), linewidth = lw) +
  geom_line(aes(y = y2, color = "d2 = 70"), linewidth = lw) +
  geom_line(aes(y = y3, color = "d2 = 80"), linewidth = lw) +
  geom_line(aes(y = y4, color = "d2 = 90"), linewidth = lw) +
  geom_line(aes(y = y5, color = "d2 = 100"), linewidth = lw) +
  geom_line(aes(y = y6, color = "d2 = 110"), linewidth = lw) +
  geom_line(aes(y = y7, color = "d2 = 120"), linewidth = lw) +
  geom_line(aes(y = y8, color = "d2 = 130"), linewidth = lw) +
  geom_line(aes(y = y9, color = "d2 = 140"), linewidth = lw) +
  geom_line(aes(y = ya, color = "d2 = 150"), linewidth = lw) +
  scale_color_manual(values = c("d2 = 50" = cols[1], "d2 = 60" = cols[3],
                                "d2 = 70" = cols[5], "d2 = 80" = cols[7],
                                "d2 = 90" = cols[9], "d2 = 100" = cols[11],
                                "d2 = 110" = cols[12], "d2 = 120" = cols[14],
                                "d2 = 130" = cols[16], "d2 = 140" = cols[18],
                                "d2 = 150" = cols[20])) +
  labs(colour = expression('d'[2]), title = title_d2, x = "Days", y = NULL) +
  coord_cartesian(ylim = c(0 , 1.2)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_d2_d


#####Três ondas / variando d da terceira onda#####

p_d3_d <- data.frame(t=t[2:length(t)], y0=diff(p_d3$y0), y1=diff(p_d3$y1), y2=diff(p_d3$y2), y3=diff(p_d3$y3),
                     y4=diff(p_d3$y4), y5=diff(p_d3$y5), y6=diff(p_d3$y6), y7=diff(p_d3$y7), y8=diff(p_d3$y8),
                     y9=diff(p_d3$y9), ya=diff(p_d3$ya))

fig_d3_d <- ggplot(p_d3_d,aes(x=t))+
  geom_line(aes(y = y0, color = "z = 50"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = 60"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = 70"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = 80"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = 90"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = 100"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = 110"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = 120"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = 130"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = 140"), linewidth = lw) +
  geom_line(aes(y = ya, color = "z = 150"), linewidth = lw) +
  scale_color_manual(values = c("z = 50" = cols[1], "z = 60" = cols[3],
                                "z = 70" = cols[5], "z = 80" = cols[7],
                                "z = 90" = cols[9], "z = 100" = cols[11],
                                "z = 110" = cols[12], "z = 120" = cols[14],
                                "z = 130" = cols[16], "z = 140" = cols[18],
                                "z = 150" = cols[20])) +
  labs(colour = NULL, title = title_d3, x = "Days", y = NULL) +
  coord_cartesian(ylim = c(0 , 1.2)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_d3_d


#####Três ondas / variando e da primeira onda#####

p_e1_d <- data.frame(t=t[2:length(t)], y0=diff(p_e1$y0), y1=diff(p_e1$y1), y2=diff(p_e1$y2), y3=diff(p_e1$y3),
                     y4=diff(p_e1$y4), y5=diff(p_e1$y5), y6=diff(p_e1$y6), y7=diff(p_e1$y7), y8=diff(p_e1$y8),
                     y9=diff(p_e1$y9), ya=diff(p_e1$ya))

fig_e1_d <- ggplot(p_e1_d,aes(x=t))+
  geom_line(aes(y = y0, color = "e1 = 4.5"), linewidth = lw) +
  geom_line(aes(y = y1, color = "e1 = 4.6"), linewidth = lw) +
  geom_line(aes(y = y2, color = "e1 = 4.7"), linewidth = lw) +
  geom_line(aes(y = y3, color = "e1 = 4.8"), linewidth = lw) +
  geom_line(aes(y = y4, color = "e1 = 4.9"), linewidth = lw) +
  geom_line(aes(y = y5, color = "e1 = 5"), linewidth = lw) +
  geom_line(aes(y = y6, color = "e1 = 5.1"), linewidth = lw) +
  geom_line(aes(y = y7, color = "e1 = 5.2"), linewidth = lw) +
  geom_line(aes(y = y8, color = "e1 = 5.3"), linewidth = lw) +
  geom_line(aes(y = y9, color = "e1 = 5.4"), linewidth = lw) +
  geom_line(aes(y = ya, color = "e1 = 5.5"), linewidth = lw) +
  scale_color_manual(values = c("e1 = 4.5" = cols[1], "e1 = 4.6" = cols[3],
                                "e1 = 4.7" = cols[5], "e1 = 4.8" = cols[7],
                                "e1 = 4.9" = cols[9], "e1 = 5" = cols[11],
                                "e1 = 5.1" = cols[12], "e1 = 5.2" = cols[14],
                                "e1 = 5.3" = cols[16], "e1 = 5.4" = cols[18],
                                "e1 = 5.5" = cols[20])) +
  labs(colour = expression('e'[1]), title = title_e1, x = "Days", y = "Deaths") +
  coord_cartesian(ylim = c(0 , 2.1)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_e1_d


#####Três ondas / variando e da segunda onda#####

p_e2_d <- data.frame(t=t[2:length(t)], y0=diff(p_e2$y0), y1=diff(p_e2$y1), y2=diff(p_e2$y2), y3=diff(p_e2$y3),
                     y4=diff(p_e2$y4), y5=diff(p_e2$y5), y6=diff(p_e2$y6), y7=diff(p_e2$y7), y8=diff(p_e2$y8),
                     y9=diff(p_e2$y9), ya=diff(p_e2$ya))

fig_e2_d <- ggplot(p_e2_d,aes(x=t))+
  geom_line(aes(y = y0, color = "e2 = 5.5"), linewidth = lw) +
  geom_line(aes(y = y1, color = "e2 = 5.6"), linewidth = lw) +
  geom_line(aes(y = y2, color = "e2 = 5.7"), linewidth = lw) +
  geom_line(aes(y = y3, color = "e2 = 5.8"), linewidth = lw) +
  geom_line(aes(y = y4, color = "e2 = 5.9"), linewidth = lw) +
  geom_line(aes(y = y5, color = "e2 = 6"), linewidth = lw) +
  geom_line(aes(y = y6, color = "e2 = 6.1"), linewidth = lw) +
  geom_line(aes(y = y7, color = "e2 = 6.2"), linewidth = lw) +
  geom_line(aes(y = y8, color = "e2 = 6.3"), linewidth = lw) +
  geom_line(aes(y = y9, color = "e2 = 6.4"), linewidth = lw) +
  geom_line(aes(y = ya, color = "e2 = 6.5"), linewidth = lw) +
  scale_color_manual(values = c("e2 = 5.5" = cols[1], "e2 = 5.6" = cols[3],
                                "e2 = 5.7" = cols[5], "e2 = 5.8" = cols[7],
                                "e2 = 5.9" = cols[9], "e2 = 6" = cols[11],
                                "e2 = 6.1" = cols[12], "e2 = 6.2" = cols[14],
                                "e2 = 6.3" = cols[16], "e2 = 6.4" = cols[18],
                                "e2 = 6.5" = cols[20])) +
  labs(title = title_e2, x = "Days", y = NULL) +
  coord_cartesian(ylim = c(0 , 2.1)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_e2_d


#####Três ondas / variando e da terceira onda#####

p_e3_d <- data.frame(t=t[2:length(t)], y0=diff(p_e3$y0), y1=diff(p_e3$y1), y2=diff(p_e3$y2), y3=diff(p_e3$y3),
                     y4=diff(p_e3$y4), y5=diff(p_e3$y5), y6=diff(p_e3$y6), y7=diff(p_e3$y7), y8=diff(p_e3$y8),
                     y9=diff(p_e3$y9), ya=diff(p_e3$ya))

fig_e3_d <- ggplot(p_e3_d,aes(x=t))+
  geom_line(aes(y = y0, color = "z = -0.5"), linewidth = lw) +
  geom_line(aes(y = y1, color = "z = -0.4"), linewidth = lw) +
  geom_line(aes(y = y2, color = "z = -0.3"), linewidth = lw) +
  geom_line(aes(y = y3, color = "z = -0.2"), linewidth = lw) +
  geom_line(aes(y = y4, color = "z = -0.1"), linewidth = lw) +
  geom_line(aes(y = y5, color = "z = 0"), linewidth = lw) +
  geom_line(aes(y = y6, color = "z = 0.1"), linewidth = lw) +
  geom_line(aes(y = y7, color = "z = 0.2"), linewidth = lw) +
  geom_line(aes(y = y8, color = "z = 0.3"), linewidth = lw) +
  geom_line(aes(y = y9, color = "z = 0.4"), linewidth = lw) +
  geom_line(aes(y = ya, color = "z = 0.5"), linewidth = lw) +
  scale_color_manual(values = c("z = -0.5" = cols[1], "z = -0.4" = cols[3],
                                "z = -0.3" = cols[5], "z = -0.2" = cols[7],
                                "z = -0.1" = cols[9], "z = 0" = cols[11],
                                "z = 0.1" = cols[12], "z = 0.2" = cols[14],
                                "z = 0.3" = cols[16], "z = 0.4" = cols[18],
                                "z = 0.5" = cols[20])) +
  labs(colour = NULL, title = title_e3, x = "Days", y = NULL) +
  coord_cartesian(ylim = c(0 , 2.1)) +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")#; fig_e3_d


#####grade com os 9 gráficos#####
G1 <- ggarrange(fig_b1_d, fig_b2_d, fig_b3_d, fig_sc_b, widths = c(2,2,2,0.35), ncol=4, nrow=1)
# title_G1 <- expression(paste("Fixed parameters: ",d[1] == 100," | ",e[1] == 5," | ",d[2] == 150," | ",e[2] == 6," | ",d[3] == 50," | ",e[3] == 7))
# title_G1 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
#                      textGrob(title_G1, x=0.5, hjust=0.5,
#                               gp=gpar(col="black", cex = 1.2)))
#G1 <- grid.arrange(title_G1, G1, heights=c(1,9))
G1 <- ggarrange(title_G1, G1, heights=c(1,9), ncol=1, nrow=2)
#G1 <- annotate_figure(G1, top = text_grob(title_G1, face = "bold", size = 14))

G2 <- ggarrange(fig_d1_d, fig_d2_d, fig_d3_d, fig_sc_d, widths = c(2,2,2,0.35), ncol=4, nrow=1)
# title_G2 <- expression(paste("Fixed parameters: ",b[1] == -4," | ",e[1] == 5," | ",b[2] == -12," | ",e[2] == 6," | ",b[3] == -8," | ",e[3] == 7))
# title_G2 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
#                      textGrob(title_G2, x=0.5, hjust=0.5,
#                               gp=gpar(col="black", cex = 1.2)))
#G2 <- grid.arrange(title_G2, G2, heights=c(1,9))
G2 <- ggarrange(title_G2, G2, heights=c(1,9), ncol=1, nrow=2)
#G2 <- annotate_figure(G2, top = text_grob(title_G2, face = "bold", size = 14))

G3 = ggarrange(fig_e1_d, fig_e2_d, fig_e3_d, fig_sc_e, widths = c(2,2,2,0.35), ncol=4, nrow=1)
# title_G3 <- expression(paste("Fixed parameters: ",b[1] == -4," | ",d[1] == 100," | ",b[2] == -12," | ",d[2] == 150," | ",b[3] == -8," | ",d[3] == 50))
# title_G3 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
#                      textGrob(title_G3, x=0.5, hjust=0.5,
#                               gp=gpar(col="black", cex = 1.2)))
#G3 <- grid.arrange(title_G3, G3, heights=c(1,9))
G3 <- ggarrange(title_G3, G3, heights=c(1,9), ncol=1, nrow=2)
#G3 <- annotate_figure(G3, top = text_grob(title_G3, face = "bold", size = 14))

grid2 <- ggarrange(G1,G2,G3,ncol=1,nrow=3);grid2



########## Gráfico do processamento dos dados ##########

##### (1) Gráfico da série acumulada (dados originais) #####

est <- "São Paulo"
cid <- "São Paulo"
variavel <- "deaths"

df <- carregar_dados(est, cid, c('2020-02-01', '2023-03-23'), variavel)
datas <- df$date
series <- df$deaths

p_acum <- data.frame(datas=datas,series=series)

Sys.setlocale("LC_ALL","English")

fig_acum <- ggplot(p_acum,aes(x=datas))+
  geom_line(aes(y = series), linewidth = 1) +
  labs(title = "(A) Cumulative deaths for the city of São Paulo", x = NULL, y = "Cumulative deaths") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y")#; fig_acum


##### (2) Gráfico da série diária (diferenciação) #####

p <- data.frame(datas=datas[2:length(datas)],series=diff(series))
fig <- ggplot(p,aes(x=datas))+
  geom_line(aes(y = series), linewidth = 0.5) +
  labs(title = "(B) Daily deaths for the city of São Paulo", x = NULL, y = "Daily deaths") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y")#; fig


##### (3) Gráfico do polinômio obtido das splines cúbicas (suavização) #####

minimos <- encontra_minimos(series)

y <- c(1, diff(series))
dof <- 50
spline_fit <- smooth.spline(y, nknots=dof)
poly <- SmoothSplineAsPiecePoly(spline_fit)

# zeros  <- solve(poly, deriv=1)
# minimos_ <- zeros[predict(poly, zeros, deriv=2)>0]
# minimos <- c(1)
# for (i in 1:(length(minimos_) - 1)) {
#   if(minimos_[i+1] - minimos_[i] > 90) {
#     minimos <- c(minimos, minimos_[i])
#   }
# }
y_poly <- predict.PiecePoly(poly, 1:length(y))

p_poly <- data.frame(datas=datas,series=y_poly)
fig_poly <- ggplot(p_poly,aes(x=datas))+
  geom_line(aes(y = series), linewidth = 1) +
  labs(title = "(C) Piecewise cubic polynomial", x = NULL, y = "Daily deaths") +
  theme_minimal() +
  scale_x_date(date_breaks = "9 months", date_labels = "%b-%Y") #+
#  easy_rotate_x_labels(angle=45, side="right")#; fig_poly


##### (4) Gráfico do polinômio com indicação dos mínimos (derivação) #####

p_minimos <- data.frame(datas=as.Date(minimos, origin=df$date[1]), y=predict.PiecePoly(poly,minimos))

p_onda1 <- data.frame(datas=p_poly$datas[as.integer(minimos[1]):as.integer(minimos[2])],y=p_poly$series[as.integer(minimos[1]):as.integer(minimos[2])])
p_onda2 <- data.frame(datas=p_poly$datas[as.integer(minimos[2]):as.integer(minimos[3])],y=p_poly$series[as.integer(minimos[2]):as.integer(minimos[3])])
p_onda3 <- data.frame(datas=p_poly$datas[as.integer(minimos[3]):as.integer(minimos[4])],y=p_poly$series[as.integer(minimos[3]):as.integer(minimos[4])])
p_onda4 <- data.frame(datas=p_poly$datas[as.integer(minimos[4]):as.integer(minimos[5])],y=p_poly$series[as.integer(minimos[4]):as.integer(minimos[5])])
p_onda5 <- data.frame(datas=p_poly$datas[as.integer(minimos[5]):length(p_poly$datas)],y=p_poly$series[as.integer(minimos[5]):length(p_poly$datas)])

fig_poly2 <- ggplot()+
  geom_line(data=p_onda1,aes(x = datas, y = y, color = "1st wave"), linewidth = 1) +
  geom_line(data=p_onda2,aes(x = datas, y = y, color = "2nd wave"), linewidth = 1) +
  geom_line(data=p_onda3,aes(x = datas, y = y, color = "3rd wave"), linewidth = 1) +
  geom_line(data=p_onda4,aes(x = datas, y = y, color = "4th wave"), linewidth = 1) +
  geom_line(data=p_onda5,aes(x = datas, y = y, color = "5th wave"), linewidth = 1) +
  scale_color_manual(values = c("1st wave" = "blue", "2nd wave" = "red",
                                "3rd wave" = "orange", "4th wave" = "cyan", "5th wave" = "green")) +
  labs(title = "(D) Wave interval estimation", x = NULL, y = NULL) +
  theme_minimal() +
  scale_x_date(date_breaks = "9 months", date_labels = "%b-%Y")+
  geom_point(data = p_minimos[2:6,1:2], mapping = aes(x=datas, y=y), color = "black", size = 1.5, show.legend = T) +
  # easy_rotate_x_labels(angle=45, side="right") +
  easy_remove_legend()#; fig_poly2

##### (5) Gráfico de N curvas log-logísticas (ajuste por intervalos) #####

N <- length(minimos)

chute_inicial <- chutes_iniciais(series, N, minimos)

b1 <- chute_inicial[1]
d1 <- chute_inicial[2]
e1 <- chute_inicial[3]
b2 <- chute_inicial[4]
d2 <- chute_inicial[5]
e2 <- chute_inicial[6]
b3 <- chute_inicial[7]
d3 <- chute_inicial[8]
e3 <- chute_inicial[9]
b4 <- chute_inicial[10]
d4 <- chute_inicial[11]
e4 <- chute_inicial[12]
b5 <- chute_inicial[13]
d5 <- chute_inicial[14]
e5 <- chute_inicial[15]

y1 <- d1 / (1 + exp(b1 * (log(1:length(datas)) - log(e1))))
y2 <- d2 / (1 + exp(b2 * (log(1:length(datas)) - log(e2))))
y3 <- d3 / (1 + exp(b3 * (log(1:length(datas)) - log(e3))))
y4 <- d4 / (1 + exp(b4 * (log(1:length(datas)) - log(e4))))
y5 <- d5 / (1 + exp(b5 * (log(1:length(datas)) - log(e5))))
curves <- data.frame(y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, datas=datas)

fig_curves <- ggplot(curves,aes(x=datas))+
  geom_line(aes(y = y1, color = "1st wave"), linewidth = 1) +
  geom_line(aes(y = y2, color = "2nd wave"), linewidth = 1) +
  geom_line(aes(y = y3, color = "3rd wave"), linewidth = 1) +
  geom_line(aes(y = y4, color = "4th wave"), linewidth = 1) +
  geom_line(aes(y = y5, color = "5th wave"), linewidth = 1) +
  #geom_line(aes(y = y6, color = "6th wave"), linewidth = 1) +
  scale_color_manual(values = c("1st wave" = "blue", "2nd wave" = "red",
                                "3rd wave" = "orange", "4th wave" = "cyan",
                                "5th wave" = "green")) +#, "6th wave" = "magenta")) +
  labs(colour = NULL, title = "(E) Log-logistic curves", x = NULL, y = "Cumulative deaths") +
  theme_minimal() +
  scale_x_date(date_breaks = "9 months", date_labels = "%b-%Y") +
  # easy_rotate_x_labels(angle=45, side="right") +
  theme(legend.position="left",#c(0.15,0.6),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill="gray90", linetype = "blank"))#; fig_curves
  

##### (6) Gráfico da soma das N curvas log-logísticas (ajuste para a série acumulada) #####
p_tend_acum <- data.frame(datas=datas,series=p_acum[['series']])
fig_tend_acum <- ggplot(p_tend_acum,aes(x=datas))+
  geom_line(aes(y = series, color="input data"), linewidth = 1) +
  geom_line(aes(y = y1+y2+y3+y4+y5+y5, color = "initial model"), linewidth = 1) +
  geom_line(aes(y = y1, color = "1st wave"), linewidth = 1) +
  geom_line(aes(y = y2, color = "2nd wave"), linewidth = 1) +
  geom_line(aes(y = y3, color = "3rd wave"), linewidth = 1) +
  geom_line(aes(y = y4, color = "4th wave"), linewidth = 1) +
  geom_line(aes(y = y5, color = "5th wave"), linewidth = 1) +
  scale_color_manual(values = c("1st wave" = "blue", "2nd wave" = "red",
                                "3rd wave" = "orange", "4th wave" = "cyan",
                                "5th wave" = "green", "initial model" = "black", "input data" = "lightpink")) +
  labs(colour = NULL, title = "(F) Cumulative deaths for the city of São Paulo (initial model)", x = NULL, y = "Cumulative deaths") +
  theme_minimal() +
  theme(legend.position="right",#c(0.1, 0.7),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill="gray90", linetype = "blank")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y")#;fig_tend_acum# +
#  easy_rotate_x_labels(angle=45, side="right")#; fig_tend_acum
#+ geom_point(data = p_minimos, mapping = aes(x=datas, y=y), color = "red", size = 2.5, show.legend = T); fig_tend


##### (7) Gráfico da soma das N curvas log-logísticas (ajuste para a série diária) #####
p_tend <- data.frame(datas=datas[2:length(datas)],series=p[['series']])
fig_tend <- ggplot(p_tend,aes(x=datas))+
  geom_line(aes(y = series, color="input data"), linewidth = 0.5) +
  geom_line(aes(y = diff(y1+y2+y3+y4+y5), color="initial model"), linewidth = 2) +
  geom_line(aes(y = diff(y1), color="1st wave"), linewidth = 1) +
  geom_line(aes(y = diff(y2), color="2nd wave"), linewidth = 1) +
  geom_line(aes(y = diff(y3), color="3rd wave"), linewidth = 1) +
  geom_line(aes(y = diff(y4), color="4th wave"), linewidth = 1) +
  geom_line(aes(y = diff(y5), color="5th wave"), linewidth = 1) +
  scale_color_manual(values = c("input data" = "lightpink", "initial model" = "black",
                                "1st wave" = "blue", "2nd wave" = "red", "3rd wave" = "orange",
                                "4th wave" = "cyan", "5th wave" = "green")) +
  labs(colour = NULL, title = "(G) Daily deaths for the city of São Paulo (initial model)", x = NULL, y = "Daily deaths") +
  theme_minimal() +
  easy_remove_legend() +
  # theme(legend.position=c(0.8, 0.8),
  #       legend.text=element_text(size=10),
  #       legend.background = element_rect(fill="gray90", linetype = "blank")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y")#+
#  easy_rotate_x_labels(angle=45, side="right"); fig_tend
  #+ geom_point(data = p_minimos, mapping = aes(x=datas, y=y), color = "red", size = 2.5, show.legend = T); fig_tend


##### Grade com os 7 gráficos #####

G1 <- ggarrange(fig_acum, fig, ncol=2, nrow=1)
title_G1 <- "Input data"
title_G1 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
                     textGrob(title_G1, x=0.5, hjust=0.5,
                              gp=gpar(col="black", cex = 1.2)))
#G1 <- grid.arrange(title_G1, G1, heights=c(1,9))
G1 <- ggarrange(title_G1, G1, heights=c(1,9), ncol=1, nrow=2)
#G1 <- annotate_figure(G1, top = text_grob(title_G1, face = "bold", size = 14))

G2 <- ggarrange(fig_poly, fig_poly2, ncol=2, nrow=1)
title_G2 <- "Smoothing"
title_G2 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
                     textGrob(title_G2, x=0.5, hjust=0.5,
                              gp=gpar(col="black", cex = 1.2)))
#G2 <- grid.arrange(title_G2, G2, heights=c(1,9))
G2 <- ggarrange(title_G2, G2, heights=c(1,9), ncol=1, nrow=2)
#G2 <- annotate_figure(G2, top = text_grob(title_G2, face = "bold", size = 14))

G3 <- fig_curves
title_G3 <- "Initial individual wave models"
title_G3 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
                     textGrob(title_G3, x=0.5, hjust=0.5,
                              gp=gpar(col="black", cex = 1.2)))
#G3 <- grid.arrange(title_G3, G3, heights=c(1,9))
G3 <- ggarrange(title_G3, G3, heights=c(1,9), ncol=1, nrow=2)
#G2 <- annotate_figure(G2, top = text_grob(title_G2, face = "bold", size = 14))

G23 <- ggarrange(G2,G3,ncol=2,nrow=1, widths=c(1.6,1))

G4 <- ggarrange(fig_tend_acum, fig_tend, ncol=2, nrow=1, widths = c(1.1,1))
title_G4 <- "Initial multiple wave model"
title_G4 <- grobTree(rectGrob(gp=gpar(col="white",fill="lightgrey")),
                     textGrob(title_G4, x=0.5, hjust=0.5,
                              gp=gpar(col="black", cex = 1.2)))
#G3 <- grid.arrange(title_G3, G3, heights=c(1,9))
G4 <- ggarrange(title_G4, G4, heights=c(1,9), ncol=1, nrow=2)
#G3 <- annotate_figure(G3, top = text_grob(title_G3, face = "bold", size = 14))

grid3 <- ggarrange(G1,G23,G4,ncol=1,nrow=3);grid3













