library(tidyverse)

prob=c(.3,.55,.45)

########################################################################

cual_maximo = function(max_vec){
  maximo = which(max(max_vec) == max_vec)
  if (length(maximo) == 1) {
    return(maximo)
  } else {return(sample(maximo, 1))}
}

########################################################################

#Punto 1

datos = matrix(rbinom(366000, 1, prob[2]), nrow = 1000, ncol = 366, byrow = T)

hist(rowSums(datos))

mean(rowSums(datos))

dates = sort(rowSums(datos))

c(mean(dates[25], dates[26]), mean(dates[975], dates[976]))


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
qbeta(.975,6,13)
qbeta(.975,18,7)

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

datos=actualizar(366,1,"greedy_prior",1,.5)





  

