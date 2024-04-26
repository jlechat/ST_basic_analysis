#Linear Time Series Assignment :
#ARIMA modelling of a time series

library(insee)
library(data.table)
library(tidyverse)
library(zoo)
library(tseries)
library(fUnitRoots)
library(astsa)
library(forecast)
library(portes)

#0- DATA IMPORTATION AND FORMATING ----

#data = as.data.table(insee::get_insee_idbank("010767584"))

data = as.data.table(insee::get_insee_idbank("010767581"))


data=data[,c("DATE", "OBS_VALUE")]
setnames(data, c("DATE", "extraction_gaz_naturel"))


data<-data[order(as.Date(data$DATE)),]



#METHODE SLIDE PROF -----
plot(diff(data[,extraction_gaz_naturel]))

#tests ? bien revoir les spécifications

pp.test(data[,extraction_gaz_naturel])
adf.test(data[,extraction_gaz_naturel])
kpss.test(data[,extraction_gaz_naturel])

adfTest(data[,extraction_gaz_naturel], type = "ct")


pp.test(diff(data[,extraction_gaz_naturel]))
adf.test(diff(data[,extraction_gaz_naturel]))
kpss.test(diff(data[,extraction_gaz_naturel]))


acf(diff(data[,extraction_gaz_naturel]))
#q=3
pacf(diff(data[,extraction_gaz_naturel]))

#il semble y avoir une saisonalité ? plusieurs ?
#p=7

#saisonnalité ?
xm=diff(data[,extraction_gaz_naturel])
des=xm-lag(xm,12)

acf(na.remove(des))
#q=3
pacf(na.remove(des))
#ca ne change rien



res37<-sarima(data[,extraction_gaz_naturel], 3,1,7, details= TRUE)
res27<-sarima(data[,extraction_gaz_naturel], 2,1,7, details= TRUE)
res17<-sarima(data[,extraction_gaz_naturel], 1,1,7, details= TRUE)
res07<-sarima(data[,extraction_gaz_naturel], 0,1,7, details= TRUE)

for (i in 0:4) {
  sarima(data[,extraction_gaz_naturel], 4,1,i, details= FALSE)
  sarima(data[,extraction_gaz_naturel], 3,1,i, details= FALSE)
  sarima(data[,extraction_gaz_naturel], 2,1,i, details= FALSE)
  sarima(data[,extraction_gaz_naturel], 1,1,i, details= FALSE)
  sarima(data[,extraction_gaz_naturel], 0,1,i, details= FALSE)
}

res30<-sarima(data[,extraction_gaz_naturel], 3,1,0, details= FALSE)
res20<-sarima(data[,extraction_gaz_naturel], 2,1,0, details= FALSE)
res10<-sarima(data[,extraction_gaz_naturel], 1,1,0, details= FALSE)
res21<-sarima(data[,extraction_gaz_naturel], 2,1,1, details= FALSE)
res11<-sarima(data[,extraction_gaz_naturel], 1,1,1, details= FALSE)
res01<-sarima(data[,extraction_gaz_naturel], 0,1,1, details= FALSE)
res32<-sarima(data[,extraction_gaz_naturel], 3,1,2, details= FALSE)
res02<-sarima(data[,extraction_gaz_naturel], 0,1,2, details= FALSE)
res33<-sarima(data[,extraction_gaz_naturel], 3,1,3, details= FALSE)
res13<-sarima(data[,extraction_gaz_naturel], 1,1,3, details= FALSE)
res04<-sarima(data[,extraction_gaz_naturel], 0,1,4, details= FALSE)
res25<-sarima(data[,extraction_gaz_naturel], 2,1,5, details= FALSE)


LjungBox(res30$fit) #rejected
LjungBox(res20$fit) #rejected
LjungBox(res10$fit) #rejected
LjungBox(res21$fit) #rejected
LjungBox(res11$fit) #rejected
LjungBox(res01$fit) #rejected
LjungBox(res32$fit) #rejected
LjungBox(res02$fit) #rejected
LjungBox(res33$fit) #rejected
LjungBox(res13$fit) #?
LjungBox(res04$fit) #rejected
LjungBox(res25$fit)



#avec PETROLE 
res20<-sarima(data[,extraction_gaz_naturel], 2,1,0, details= FALSE)
res10<-sarima(data[,extraction_gaz_naturel], 1,1,0, details= FALSE)
res21<-sarima(data[,extraction_gaz_naturel], 2,1,1, details= FALSE)
res01<-sarima(data[,extraction_gaz_naturel], 0,1,1, details= FALSE)
res32<-sarima(data[,extraction_gaz_naturel], 3,1,2, details= FALSE)
res33<-sarima(data[,extraction_gaz_naturel], 3,1,3, details= FALSE)
res43<-sarima(data[,extraction_gaz_naturel], 4,1,3, details= FALSE)


LjungBox(res20$fit) #rejected
LjungBox(res10$fit) #REJECTED
LjungBox(res21$fit) #REJECTED
LjungBox(res01$fit) #REJECTED
LjungBox(res33$fit) #rejected

LjungBox(res32$fit) #ACCEPTED

LjungBox(res43$fit) #ACCEPTED


#checkresiduals(res20$fit)

AIC(res32$fit)
AIC(res43$fit)

BIC(res32$fit)
BIC(res43$fit)

#autoplot(forecast(res20$fit))
