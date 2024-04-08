library(tidyverse)


prob=c(.3,.55,.45)
########################################################################
random=function(y,t){
  cual=floor(runif(1,1,4))
  vector=c(rbinom(1,1,prob[cual]),cual)
  return(vector)}
########################################################################
softmax=function(tasas,temp){
  p=exp(tasas/temp)/sum(exp(tasas/temp))
  return(p)}

soft=function(y,temp){
  tasa=y[1:3]/y[4:6]
  for (k in 1:length(tasa)) {
    if (is.na(tasa[k])) {
      tasa[k]=0
    }
  }
  p=softmax(tasa,temp)
  cual=sample(c(1,2,3),size = 1,prob = p)
  vector=c(rbinom(1,1,prob[cual]),cual)
  return(vector)
}
########################################################################

thompson=function(y,t){
  maquina1=rbeta(1,2+y[1],2+y[4]-y[1])
  maquina2=rbeta(1,2+y[2],2+y[5]-y[2])
  maquina3=rbeta(1,2+y[3],2+y[6]-y[3])
  cual=which.max(c(maquina1,maquina2,maquina3))
  vector=c(rbinom(1,1,prob[cual]),cual)
  return(vector)
}

########################################################################

upper=function(y,t){
  maquina1=qbeta(.95,2+y[1],2+y[4]-y[1])
  maquina2=qbeta(.95,2+y[2],2+y[5]-y[2])
  maquina3=qbeta(.95,2+y[3],2+y[6]-y[3])
  if (maquina1==maquina2 & maquina1==maquina3 ){
    cual=floor(runif(1,1,4))
  }else{cual=which.max(c(maquina1,maquina2,maquina3))} 
  vector=c(rbinom(1,1,prob[cual]),cual)
  return(vector)
}
########################################################################
greedy_prior=function(y,t){
  maquina1=(2+y[1])/(2+y[4]-y[1])
  maquina2=(2+y[2])/(2+y[5]-y[2])
  maquina3=(2+y[3])/(2+y[6]-y[3])
  if (maquina1==maquina2 & maquina1==maquina3 ){
    cual=floor(runif(1,1,4))
  }else{cual=which.max(c(maquina1,maquina2,maquina3))} 
  vector=c(rbinom(1,1,prob[cual]),cual)
  return(vector)
}
########################################################################
greedy_tasa=function(y,t){
  if(sum(y[4:6])<=30){
    cual=floor(runif(1,1,4))
    vector=c(rbinom(1,1,prob[cual]),cual)
  }else{
    cual=which.max(c(y[1:3])/c(y[4:6]))
    vector=c(rbinom(1,1,prob[cual]),cual)
  }
  return(vector)
}



########################################################################

e_greedy=function(y,e){
  which=rbinom(1,1,e)
  if (which==1) {
    greedy_tasa(y)
  }else{random(y)}}



########################################################################
actualizar=function(dias,muestras,metodo,constante){
  lista=list(random=random,greedy_tasa=greedy_tasa,greedy_prior=greedy_prior,e_greedy=e_greedy,softmax=soft,upper=upper,thompson=thompson)
  metodo=lista[[metodo]]
  matriz=matrix(0,dias*muestras,10)
  for (j in 1:muestras) {
    y=c(0,0,0,0,0,0,j)
    for (i in 1:dias) {
      x=metodo(y,constante)
      y[x[2]]=y[x[2]]+x[1]
      y[x[2]+3]=y[x[2]+3]+1
      matriz[i+(dias*(j-1)),]=c(x,y,i)
    }
  }
  datos=as.data.frame(matriz)
  colnames(datos)=c("Exito","Maquina","Maquina1","Maquina2","Maquina3","Maquina1Total","Maquina2Total","Maquina3Total","Muestra","Dia")
  return(datos)
}

