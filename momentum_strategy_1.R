require(lubridate)
require(pracma)
require(googleVis)


setwd("C:/Users/daniel.manzanares/Desktop/Literatura/IEB/Basico/R")
load("VIX_and_SP500.RData")

###EXPLORATION###

par(mfrow = c(2,1))
plot(SP500$date[which(year(SP500$date)>2019)],SP500$close[which(year(SP500$date)>2019)],main="SP500",ylab="Precio",xlab="Fecha",type="l")
plot(vix$date[which(year(vix$date)>2019)],vix$close[which(year(vix$date)>2019)],main="VIX",ylab="Precio",xlab="Fecha",type="l")

###SYSTEM###

TablaMaestra <- SP500[,c("date","close")]
TablaMaestra$VixClose <- vix$close[which(vix$date%in%SP500$date)]
colnames(TablaMaestra) <- c("Fecha","SP","VIX")
TablaMaestra$SPin <- c(0,diff(TablaMaestra$SP))  #Return abs
TablaMaestra$VIXin <- c(0,diff(TablaMaestra$VIX))   #Return abs

#Histograma de los Returns

hist(TablaMaestra$SPin,breaks = 300,col="red",main="Distr_SP")
hist(TablaMaestra$VIXin,breaks = 300,col="red",main="Distr_VIX")


#LAGWIN>MOMENDAY

lagwin <- 6 #Ventana de retraso para analizar la correlación
momenday <- 5 #Días de inercia (momentum)


#

TablaMaestra$Cor<-0 #Creamos la columna para el factor de correlación

for (i in (lagwin + 1) : nrow(TablaMaestra)) {
  
  TablaMaestra$Cor[i] <- cor(TablaMaestra$SPin[(i-lagwin):(i-1)], TablaMaestra$VIXin[(i-lagwin):(i-1)]) 
  
}

par(mfrow=c(3,1))
plot(SP500$date[which(year(SP500$date)>2019)], SP500$close[which(year(SP500$date) > 2019)], main = "SP500", ylab = "Precio",xlab =" Fecha",type = "l")
plot(vix$date[which(year(vix$date) > 2019)], vix$close[which(year(vix$date) > 2019)], main ="VIX", ylab = "Precio", xlab = "Fecha",type = "l")
plot(TablaMaestra$Cor[which(year(TablaMaestra$Fecha) > 2019)], type = "l", main = paste0("Correlacion_periodo: ", lagwin))

### FUNCIÖN DE SEÑAL ....................................########################################

#señal <- function(corthres){
  lagwin=6
  momenday=3
  TablaMaestra$Signal <- 0 #Esta columna me servirá para anotarme las señales de compra-venta (según sean +1 o -1)
  corthres =  -0.5


señal <- function(corthres)  {
  for(i in (lagwin + 1) : nrow(TablaMaestra)){
  
  #CONDICION CORRELACION
  if(TablaMaestra$Cor[i] < corthres & TablaMaestra$Cor[i] > corthres -1){ #Si hay una correlación negativa por encima de mi umbral exigido
    
    #CONDICION MOMENTUM
    if(sum(sign(TablaMaestra$SPin[(i-momenday):(i-1)])) == -momenday){ #Si SP500 lleva varios días con inercia de caída, cumple esta condición (la función "sign" devuelve +-1)
      
      TablaMaestra$Signal[i] = 1 #Me anoto señal de venta para el SP para comprar al día siguiente
      
    }
    
    #CONDICION CORRELACION
  }else if(TablaMaestra$Cor[i] < corthres -0.05){
    
    #CONDICION MOMENTUM
    if(sum(sign(TablaMaestra$SPin[(i-momenday):(i-1)])) == momenday){ #Si SP500 lleva varios días con inercia de subida, cumple esta condición (la función "sign" devuelve +-1)
      
      TablaMaestra$Signal[i] = -1 #Me anoto señal de compra para el SP para vender al día siguiente
      
    }else{TablaMaestra$Signal[i] = 0} #No anoto señal de compra 
    
  }else{
    
    TablaMaestra$Signal[i] = 0 #Si la correlación es justo el umbral, no anoto ninguna señal
  }
}
  
  TablaMaestra$PNLSystem<-TablaMaestra$Signal*TablaMaestra$SPin 
  TablaMaestra$PNLAcum<-cumsum(TablaMaestra$PNLSystem)
  
  
  TablaMaestra$Return[1] = 0         #Revissar esto
  for (i in 48:nrow(TablaMaestra)){
    if (TablaMaestra$PNLAcum[i] != TablaMaestra$PNLAcum[i-1]){
      TablaMaestra$Return[i] = TablaMaestra$PNLAcum[i]/TablaMaestra$PNLAcum[i-1] -1
    }else{
      TablaMaestra$Return[i] = 0
    }
    
  }
  
  TablaMaestra$ReturnSP[1] = 0
  for (i in 2:nrow(TablaMaestra)){
    TablaMaestra$ReturnSP[i] = TablaMaestra$SP[i]/TablaMaestra$SP[i-1] -1
  }

#plot(TablaMaestra$Fecha,TablaMaestra$Return, type = 'l')
  
  return(mean(TablaMaestra$Return[which(TablaMaestra$Fecha > '1991-01-01')])*250)
}



señal(-0.6)
#### #######################
corthres <- seq(from = -0.8, to = -0.4, by = 0.01)  # Creamos vector con las condiciones qu
corthres <- as.data.frame(corthres)
corthres$return = as.data.frame(apply(corthres, MARGIN = 1, FUN = señal ))

colnames(corthres) <- c("Rentabilidad","Condición_correlación")

