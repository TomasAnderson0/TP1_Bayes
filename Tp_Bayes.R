  library(tidyverse)
  library(ggplot2)

prob=c(.3,.55,.45)

########################################################################

cual_maximo = function(max_vec){
  maximo = which(max(max_vec) == max_vec)
  if (length(maximo) == 1) {
    return(maximo)
  } else {return(sample(maximo, 1))}
}

########################################################################

set.seed(69)


#Punto 1

datos = matrix(rbinom(366000, 1, prob[2]), nrow = 1000, ncol = 366, byrow = T)

ggplot()
x=rowSums(datos)
victor = as.data.frame(x)

tema = theme(panel.grid = element_line(color = "lightgrey"),
             panel.background = element_blank(),
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
  geom_line(aes(x=Dia, y = Maquina1Total, colour="yellow"))+
  geom_line(aes(x=Dia, y = Maquina2Total, colour="blue"))+
  geom_line(aes(x=Dia, y = Maquina3Total, colour="green"))

ggplot(datos_random_dia)+
  geom_line(aes(x=Dia, y = Maquina1+Maquina2+Maquina3, color="red"))
#to do: distribucion a posteriori



#Maquina 1
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina1[61*i],2-datos_random_dia$Maquina1[61*i]+datos_random_dia$Maquina1Total[61*i]))
  }


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color=rgb(0.55, 0, 0)))+
  geom_line(aes(x=seq,y=Dia61,color=rgb(0.8, 0, 0)))+
  geom_line(aes(x=seq,y=Dia122))+
  geom_line(aes(x=seq,y=Dia183))+
  geom_line(aes(x=seq,y=Dia244))+
  geom_line(aes(x=seq,y=Dia305))+
  geom_line(aes(x=seq,y=Dia366))
  

#Maquina 2
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina2[61*i],2-datos_random_dia$Maquina2[61*i]+datos_random_dia$Maquina2Total[61*i]))
}


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color=rgb(0.55, 0, 0)))+
  geom_line(aes(x=seq,y=Dia61,color=rgb(0.8, 0, 0)))+
  geom_line(aes(x=seq,y=Dia122))+
  geom_line(aes(x=seq,y=Dia183))+
  geom_line(aes(x=seq,y=Dia244))+
  geom_line(aes(x=seq,y=Dia305))+
  geom_line(aes(x=seq,y=Dia366))

#Maquina 3
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina3[61*i],2-datos_random_dia$Maquina3[61*i]+datos_random_dia$Maquina3Total[61*i]))
}


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color=rgb(0.55, 0, 0)))+
  geom_line(aes(x=seq,y=Dia61,color=rgb(0.8, 0, 0)))+
  geom_line(aes(x=seq,y=Dia122))+
  geom_line(aes(x=seq,y=Dia183))+
  geom_line(aes(x=seq,y=Dia244))+
  geom_line(aes(x=seq,y=Dia305))+
  geom_line(aes(x=seq,y=Dia366))










datos_random_año=actualizar(366,1000)

datos_random_filter=filter(datos_random_año, Dia == 366)

Exitos_random_totales=rowSums(datos_random_filter[,3:5])

hist(Exitos_random_totales)

Media_random_totales=mean(Exitos_random_totales)

Ordenados_random_totales = sort(Exitos_random_totales)

Q_random_totales=c(mean(Ordenados_random_totales[25], Ordenados_random_totales[26]), 
  mean(Ordenados_random_totales[975], Ordenados_random_totales[976]))


###################################################################

datos_greedy_tasa_dia=actualizar(366,1,"greedy_tasa")

ggplot(datos_greedy_tasa_dia)+
  geom_line(aes(x=Dia, y = Maquina1Total, colour="yellow"))+
  geom_line(aes(x=Dia, y = Maquina2Total, colour="blue"))+
  geom_line(aes(x=Dia, y = Maquina3Total, colour="green"))

ggplot(datos_greedy_tasa_dia)+
  geom_line(aes(x=Dia, y = Maquina1+Maquina2+Maquina3, color="red"))
#to do: distribucion a posteriori



#Maquina 1
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina1[61*i],2-datos_random_dia$Maquina1[61*i]+datos_random_dia$Maquina1Total[61*i]))
}

datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color=rgb(0.55, 0, 0)))+
  geom_line(aes(x=seq,y=Dia61,color=rgb(0.8, 0, 0)))+
  geom_line(aes(x=seq,y=Dia122))+
  geom_line(aes(x=seq,y=Dia183))+
  geom_line(aes(x=seq,y=Dia244))+
  geom_line(aes(x=seq,y=Dia305))+
  geom_line(aes(x=seq,y=Dia366))


#Maquina 2
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina2[61*i],2-datos_random_dia$Maquina2[61*i]+datos_random_dia$Maquina2Total[61*i]))
}


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color=rgb(0.55, 0, 0)))+
  geom_line(aes(x=seq,y=Dia61,color=rgb(0.8, 0, 0)))+
  geom_line(aes(x=seq,y=Dia122))+
  geom_line(aes(x=seq,y=Dia183))+
  geom_line(aes(x=seq,y=Dia244))+
  geom_line(aes(x=seq,y=Dia305))+
  geom_line(aes(x=seq,y=Dia366))

#Maquina 3
datos=matrix(seq(0,1,.001),nrow = 1001)
datos=cbind(datos,dbeta(seq(0,1,.001),2,2))
for (i in 1:6) {
  datos=cbind(datos,dbeta(seq(0,1,.001),2+datos_random_dia$Maquina3[61*i],2-datos_random_dia$Maquina3[61*i]+datos_random_dia$Maquina3Total[61*i]))
}


datos = as.data.frame(datos)
colnames(datos) = c("seq","Dia0", "Dia61", "Dia122", "Dia183", "Dia244", "Dia305",
                    "Dia366")

ggplot(datos)+geom_line(aes(x=seq,y=Dia0,color=rgb(0.55, 0, 0)))+
  geom_line(aes(x=seq,y=Dia61,color=rgb(0.8, 0, 0)))+
  geom_line(aes(x=seq,y=Dia122))+
  geom_line(aes(x=seq,y=Dia183))+
  geom_line(aes(x=seq,y=Dia244))+
  geom_line(aes(x=seq,y=Dia305))+
  geom_line(aes(x=seq,y=Dia366))










datos_greedy_tasa_año=actualizar(366,1000,"greedy_tasa")

datos_greedy_tasa_filter=filter(datos_greedy_tasa_año, Dia == 366)

Exitos_greedy_tasa_totales=rowSums(datos_greedy_tasa_filter[,3:5])

hist(Exitos_greedy_tasa_totales)

Media_greedy_tasa_totales=mean(Exitos_greedy_tasa_totales)

Ordenados_greedy_tasa_totales = sort(Exitos_greedy_tasa_totales)

Q_greedy_tasa_totales=c(mean(Ordenados_random_totales[25], Ordenados_random_totales[26]), 
                   mean(Ordenados_random_totales[975], Ordenados_random_totales[976]))







########################################################################

privilegio = function(y, temp, e){
  return(c(rbinom(1, 1, prob[2]), 2))
}


########################################################################
random = function(y, temp, e){
  cual = floor(runif(1,1,4))
  vector = c(rbinom(1, 1, prob[cual]), cual)
  return(vector)}
########################################################################
softmax = function(tasas, temp, e){
  p = exp(tasas / temp) / sum(exp(tasas / temp))
  return(p)}

soft = function(y, temp, e){
  tasa = y[1:3] / y[4:6]
  for (k in 1:length(tasa)) {
    if (is.na(tasa[k])) {
      tasa[k] = 0
    }
  }
  p = softmax(tasa, temp)
  cual = sample(c(1, 2, 3), size = 1, prob = p)
  vector = c(rbinom(1, 1, prob[cual]), cual)
  return(vector)
}
########################################################################

thompson = function(y, temp, e){
  maquina1 = rbeta(1, 2 + y[1], 2 + y[4] - y[1])
  maquina2 = rbeta(1, 2 + y[2], 2 + y[5] - y[2])
  maquina3 = rbeta(1, 2 + y[3], 2 + y[6] - y[3])
  cual = cual_maximo(c(maquina1, maquina2, maquina3))
  vector = c(rbinom(1, 1, prob[cual]), cual)
  return(vector)
}

########################################################################


upper = function(y, temp, e){
  maquina1 = qbeta(.975, 2 + y[1], 2 + y[4] - y[1])
  maquina2 = qbeta(.975, 2 + y[2], 2 + y[5] - y[2])
  maquina3 = qbeta(.975, 2 + y[3], 2 + y[6] - y[3])
  cual = cual_maximo(c(maquina1, maquina2, maquina3))
  vector = c(rbinom(1, 1, prob[cual]), cual)
  return(vector)
}
########################################################################
greedy_prior = function(y, temp, e){
  maquina1 = (2 + y[1]) / (4 + y[4])
  maquina2 = (2 + y[2]) / (4 + y[5])
  maquina3 = (2 + y[3]) / (4 + y[6])
  cual = cual_maximo(c(maquina1, maquina2, maquina3)) 
  vector = c(rbinom(1, 1, prob[cual]), cual)
  return(vector)
}
########################################################################
greedy_tasa = function(y, temp, e){
  if (y[3] == 0) {
    if (y[2] == 0) {
      if (y[1] == 0) {
        vector = c(rbinom(1, 1, prob[1]), 1)
        return(vector)
      }
      vector = c(rbinom(1, 1, prob[2]), 2)
      return(vector)
      }
    vector = c(rbinom(1, 1, prob[3]), 3)
    return(vector)
    }
  cual = cual_maximo(c(y[1:3]) / c(y[4:6]))
  vector = c(rbinom(1, 1, prob[cual]), cual)
  return(vector)
}
  
  



########################################################################

e_greedy=function(y, temp, e){
  which=rbinom(1, 1, e)
  if (which == 1) {
    greedy_tasa(y)
  } else {random(y)}
  }



########################################################################
actualizar=function(dias = 366,muestras = 1,metodo = "random", temp = 1, e = .5){
  lista=list(random = random, greedy_tasa = greedy_tasa ,greedy_prior = greedy_prior,
             e_greedy = e_greedy, softmax = soft, upper = upper, thompson = thompson)
  metodo = lista[[metodo]]
  matriz = matrix(0, dias*muestras, 10)
  for (j in 1:muestras) {
       y = c(0, 0, 0, 0, 0, 0, j)
       for (i in 1:dias) {
          x=metodo(y, temp, e)
          y[x[2]] = y[x[2]] + x[1]
          y[x[2]+3] = y[x[2] + 3] + 1
          matriz[i+(dias*(j-1)),] = c(x, y, i)
    }
  }
  datos = as.data.frame(matriz)
  colnames(datos) = c("Exito", "Maquina", "Maquina1", "Maquina2", "Maquina3", "Maquina1Total",
                    "Maquina2Total", "Maquina3Total", "Muestra", "Dia")
  return(datos)
}






  

