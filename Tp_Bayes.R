library(tidyverse)
library(ggplot2)
library(gridExtra)
source("Funciones.R")


prob=c(.3,.55,.45)



set.seed(69)




#Punto 1

datos = matrix(rbinom(366000, 1, prob[2]), nrow = 1000, ncol = 366, byrow = T)


x=rowSums(datos)
victor = as.data.frame(x)

tema = theme(panel.grid = element_line(color = "lightgrey"),
             panel.background = element_rect(fill = "#f8f8f8"),
             axis.line = element_line(color = "black"))

ggplot(victor,aes(x))+geom_histogram(fill = "#FF8F54", color = "black",binwidth = 4) +
  scale_y_continuous(expand = expand_scale(add = c(0, 0)),name = "Frecuencia absoluta") +
  scale_x_continuous(breaks = seq(168,232,8),name = "Unidades monetarias")+ tema
 







mean(rowSums(datos))

dates = sort(rowSums(datos))

c(mean(dates[25], dates[26]), mean(dates[975], dates[976]))

########################################################################
#Random

datos_random_dia=actualizar(366,1)

ggplot(datos_random_dia)+
  geom_line(aes(x=Dia, y = Maquina1Total, colour="Maquina 1"))+
  geom_line(aes(x=Dia, y = Maquina2Total, colour="Maquina 2"))+
  geom_line(aes(x=Dia, y = Maquina3Total, colour="Maquina 3"))+
  tema+
  scale_y_continuous(name="Cantidad de veces jugadas",breaks = c(seq(0,140,20),366),limits = c(0,140))+
  scale_x_continuous(name="Dias",breaks = seq(0,366,61)) +
  scale_color_discrete(breaks=c("Maquina 1", "Maquina 2", "Maquina 3"),type=c("purple", "green","blue"))


ggplot(datos_random_dia)+
  geom_line(aes(x=Dia, y = Maquina1+Maquina2+Maquina3), color="red")+
  scale_y_continuous(name="Unidades monetarias ganadas",breaks = c(seq(0,240,30)),limits = c(0,240))+
  scale_x_continuous(name="Dias",breaks = seq(0,366,61)) +tema


#Maquina 1
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina1[61*i],2-datos_random_dia$Maquina1[61*i]+datos_random_dia$Maquina1Total[61*i]))
  }


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

colores=c("#ffe500","#ffa200","#ff8000","#ff5500","#ff3600","#ff0000","#ffc500")


ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color="Dia 0"))+
  geom_line(aes(x=seq,y=Dia61,color="Dia 61"))+
  geom_line(aes(x=seq,y=Dia122,color="Dia 122"))+
  geom_line(aes(x=seq,y=Dia183,color="Dia 183"))+
  geom_line(aes(x=seq,y=Dia244,color="Dia 244"))+
  geom_line(aes(x=seq,y=Dia305,color="Dia 305"))+
  geom_line(aes(x=seq,y=Dia366, color ="Dia 366"))+
  tema+
  scale_y_continuous(name="Credibilidad")+
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())+
  scale_x_continuous(name="Probabilidad de éxito") +
  scale_color_discrete(breaks=c("Dia 0", "Dia 61", "Dia 122", "Dia 183", "Dia 244","Dia 305","Dia 366"),type=colores,name="Días")
  
  #Maquina 2
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina2[61*i],2-datos_random_dia$Maquina2[61*i]+datos_random_dia$Maquina2Total[61*i]))
}


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color="Dia 0"))+
  geom_line(aes(x=seq,y=Dia61,color="Dia 61"))+
  geom_line(aes(x=seq,y=Dia122,color="Dia 122"))+
  geom_line(aes(x=seq,y=Dia183,color="Dia 183"))+
  geom_line(aes(x=seq,y=Dia244,color="Dia 244"))+
  geom_line(aes(x=seq,y=Dia305,color="Dia 305"))+
  geom_line(aes(x=seq,y=Dia366, color ="Dia 366"))+
  tema+
  scale_y_continuous(name="Credibilidad")+
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())+
  scale_x_continuous(name="Probabilidad de éxito") +
  scale_color_discrete(breaks=c("Dia 0", "Dia 61", "Dia 122", "Dia 183", "Dia 244","Dia 305","Dia 366"),type=colores)


#Maquina 3
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina3[61*i],2-datos_random_dia$Maquina3[61*i]+datos_random_dia$Maquina3Total[61*i]))
}


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color="Dia 0"))+
  geom_line(aes(x=seq,y=Dia61,color="Dia 61"))+
  geom_line(aes(x=seq,y=Dia122,color="Dia 122"))+
  geom_line(aes(x=seq,y=Dia183,color="Dia 183"))+
  geom_line(aes(x=seq,y=Dia244,color="Dia 244"))+
  geom_line(aes(x=seq,y=Dia305,color="Dia 305"))+
  geom_line(aes(x=seq,y=Dia366, color ="Dia 366"))+
  tema+
  scale_y_continuous(name="Credibilidad")+
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())+
  scale_x_continuous(name="Probabilidad de éxito") +
  scale_color_discrete(breaks=c("Dia 0", "Dia 61", "Dia 122", "Dia 183", "Dia 244","Dia 305","Dia 366"),type=colores)








datos_random_año=actualizar(366,1000)

datos_random_filter=filter(datos_random_año, Dia == 366)

Exitos_random_totales=as.data.frame(rowSums(datos_random_filter[,3:5]))
colnames(Exitos_random_totales)="x"


ggplot(Exitos_random_totales,aes(x))+geom_histogram(fill = "#FF8F54", color = "black",binwidth = 4) +
  scale_y_continuous(expand = expand_scale(add = c(0, 0)),name = "Frecuencia absoluta") +
  scale_x_continuous(breaks = seq(128,192,8),name = "Unidades monetarias")+ tema

Media_random_totales=mean(Exitos_random_totales$x)

Ordenados_random_totales = sort(Exitos_random_totales$x)

Q_random_totales=c(mean(Ordenados_random_totales[25], Ordenados_random_totales[26]), 
  mean(Ordenados_random_totales[975], Ordenados_random_totales[976]))





###################################################################
set.seed(69)

datos_greedy_tasa_dia=actualizar(366,1,"greedy_tasa")

ggplot(datos_greedy_tasa_dia)+
  geom_line(aes(x=Dia, y = Maquina1Total, colour="Maquina 1"))+
  geom_line(aes(x=Dia, y = Maquina2Total, colour="Maquina 2"))+
  geom_line(aes(x=Dia, y = Maquina3Total, colour="Maquina 3"))+
  tema+
  scale_y_continuous(name="Cantidad de veces jugadas",breaks = c(seq(0,320,40),366))+
  scale_x_continuous(name="Dias",breaks = seq(0,366,61)) +
  scale_color_discrete(breaks=c("Maquina 1", "Maquina 2", "Maquina 3"),type=c("purple", "green","blue"))

ggplot(datos_greedy_tasa_dia)+
  geom_line(aes(x=Dia, y = Maquina1+Maquina2+Maquina3), color="red")+
  scale_y_continuous(name="Unidades monetarias ganadas",breaks = c(seq(0,240,30)),limits = c(0,240))+
  tema+
  scale_x_continuous(name="Dias",breaks = seq(0,366,61))


#Maquina 1
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_greedy_tasa_dia$Maquina1[61*i],2-datos_greedy_tasa_dia$Maquina1[61*i]+datos_greedy_tasa_dia$Maquina1Total[61*i]))
}

datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color="Dia 0"))+
  geom_line(aes(x=seq,y=Dia61,color="Dia 61"))+
  geom_line(aes(x=seq,y=Dia122,color="Dia 122"))+
  geom_line(aes(x=seq,y=Dia183,color="Dia 183"))+
  geom_line(aes(x=seq,y=Dia244,color="Dia 244"))+
  geom_line(aes(x=seq,y=Dia305,color="Dia 305"))+
  geom_line(aes(x=seq,y=Dia366, color ="Dia 366"))+
  tema+
  scale_y_continuous(name="Credibilidad")+
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())+
  scale_x_continuous(name="Probabilidad de éxito") +
  scale_color_discrete(breaks=c("Dia 0", "Dia 61", "Dia 122", "Dia 183", "Dia 244","Dia 305","Dia 366"),type=colores,name="Días")



#Maquina 2
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_greedy_tasa_dia$Maquina2[61*i],2-datos_greedy_tasa_dia$Maquina2[61*i]+datos_greedy_tasa_dia$Maquina2Total[61*i]))
}


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color="Dia 0"))+
  geom_line(aes(x=seq,y=Dia61,color="Dia 61"))+
  geom_line(aes(x=seq,y=Dia122,color="Dia 122"))+
  geom_line(aes(x=seq,y=Dia183,color="Dia 183"))+
  geom_line(aes(x=seq,y=Dia244,color="Dia 244"))+
  geom_line(aes(x=seq,y=Dia305,color="Dia 305"))+
  geom_line(aes(x=seq,y=Dia366, color ="Dia 366"))+
  tema+
  scale_y_continuous(name="Credibilidad")+
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())+
  scale_x_continuous(name="Probabilidad de éxito") +
  scale_color_discrete(breaks=c("Dia 0", "Dia 61", "Dia 122", "Dia 183", "Dia 244","Dia 305","Dia 366"),type=colores,name="Días")


#Maquina 3
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_greedy_tasa_dia$Maquina3[61*i],2-datos_greedy_tasa_dia$Maquina3[61*i]+datos_greedy_tasa_dia$Maquina3Total[61*i]))
}


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color="Dia 0"))+
  geom_line(aes(x=seq,y=Dia61,color="Dia 61"))+
  geom_line(aes(x=seq,y=Dia122,color="Dia 122"))+
  geom_line(aes(x=seq,y=Dia183,color="Dia 183"))+
  geom_line(aes(x=seq,y=Dia244,color="Dia 244"))+
  geom_line(aes(x=seq,y=Dia305,color="Dia 305"))+
  geom_line(aes(x=seq,y=Dia366, color ="Dia 366"))+
  tema+
  scale_y_continuous(name="Credibilidad")+
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())+
  scale_x_continuous(name="Probabilidad de éxito") +
  scale_color_discrete(breaks=c("Dia 0", "Dia 61", "Dia 122", "Dia 183", "Dia 244","Dia 305","Dia 366"),type=colores,name="Días")




datos_greedy_tasa_año=actualizar(366,1000,"greedy_tasa")

datos_greedy_tasa_filter=filter(datos_greedy_tasa_año, Dia == 366)

Exitos_greedy_tasa_totales=as.data.frame(rowSums(datos_greedy_tasa_filter[,3:5]))

colMeans(datos_greedy_tasa_filter[,6:8])

colnames(Exitos_greedy_tasa_totales)="x"


ggplot(Exitos_greedy_tasa_totales,aes(x))+geom_histogram(fill = "#FF8F54", color = "black",binwidth = 4) +
  scale_y_continuous(expand = expand_scale(add = c(0, 0)),name = "Frecuencia absoluta") +
  scale_x_continuous(breaks = seq(128,192,8),name = "Unidades monetarias")+ tema

Media_greedy_tasa_totales=mean(Exitos_greedy_tasa_totales)

Ordenados_greedy_tasa_totales = sort(Exitos_greedy_tasa_totales)

Q_greedy_tasa_totales=c(mean(Ordenados_random_totales[25], Ordenados_random_totales[26]), 
                   mean(Ordenados_random_totales[975], Ordenados_random_totales[976]))



  

