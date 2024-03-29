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

lagwin <- 6 #Ventana de retraso para analizar la correlaci�n
momenday <- 5 #D�as de inercia (momentum)


#

TablaMaestra$Cor<-0 #Creamos la columna para el factor de correlaci�n

for (i in (lagwin + 1) : nrow(TablaMaestra)) {
  
  TablaMaestra$Cor[i] <- cor(TablaMaestra$SPin[(i-lagwin):(i-1)], TablaMaestra$VIXin[(i-lagwin):(i-1)]) 
  
}

par(mfrow=c(3,1))
plot(SP500$date[which(year(SP500$date)>2019)], SP500$close[which(year(SP500$date) > 2019)], main = "SP500", ylab = "Precio",xlab =" Fecha",type = "l")
plot(vix$date[which(year(vix$date) > 2019)], vix$close[which(year(vix$date) > 2019)], main ="VIX", ylab = "Precio", xlab = "Fecha",type = "l")
plot(TablaMaestra$Cor[which(year(TablaMaestra$Fecha) > 2019)], type = "l", main = paste0("Correlacion_periodo: ", lagwin))

### FUNCI�N DE SE�AL ....................................########################################

#se�al <- function(corthres){
  lagwin=6
  momenday=3
  TablaMaestra$Signal <- 0 #Esta columna me servir� para anotarme las se�ales de compra-venta (seg�n sean +1 o -1)
  corthres =  -0.5


se�al <- function(corthres)  {
  for(i in (lagwin + 1) : nrow(TablaMaestra)){
  
  #CONDICION CORRELACION
  if(TablaMaestra$Cor[i] < corthres & TablaMaestra$Cor[i] > corthres -1){ #Si hay una correlaci�n negativa por encima de mi umbral exigido
    
    #CONDICION MOMENTUM
    if(sum(sign(TablaMaestra$SPin[(i-momenday):(i-1)])) == -momenday){ #Si SP500 lleva varios d�as con inercia de ca�da, cumple esta condici�n (la funci�n "sign" devuelve +-1)
      
      TablaMaestra$Signal[i] = 1 #Me anoto se�al de venta para el SP para comprar al d�a siguiente
      
    }
    
    #CONDICION CORRELACION
  }else if(TablaMaestra$Cor[i] < corthres -0.05){
    
    #CONDICION MOMENTUM
    if(sum(sign(TablaMaestra$SPin[(i-momenday):(i-1)])) == momenday){ #Si SP500 lleva varios d�as con inercia de subida, cumple esta condici�n (la funci�n "sign" devuelve +-1)
      
      TablaMaestra$Signal[i] = -1 #Me anoto se�al de compra para el SP para vender al d�a siguiente
      
    }else{TablaMaestra$Signal[i] = 0} #No anoto se�al de compra 
    
  }else{
    
    TablaMaestra$Signal[i] = 0 #Si la correlaci�n es justo el umbral, no anoto ninguna se�al
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



se�al(-0.6)
#### #######################
corthres <- seq(from = -0.8, to = -0.4, by = 0.01)  # Creamos vector con las condiciones qu
corthres <- as.data.frame(corthres)
corthres$return = as.data.frame(apply(corthres, MARGIN = 1, FUN = se�al ))

colnames(corthres) <- c("Rentabilidad","Condici�n_correlaci�n")

