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
library(ellipse)
library(stargazer)


######################################
# 0- DATA IMPORTATION AND FORMATING -----
######################################

data = as.data.table(insee::get_insee_idbank("010767581"))
#we get the data directly from Insee using it ID bank (010767581)
#https://www.alisse2.insee.fr/fr/statistiques/serie/010767581
#Indice CVS-CJO de la production industrielle (base 100 en 2021) - Extraction de pétrole brut (NAF rév. 2, niveau classe, poste 06.10) 

data=data[,c("DATE", "OBS_VALUE")] #we keep only relevant colunms
setnames(data, c("DATE", "oil_extrac")) #we rename its correctly


data<-data[order(as.Date(data$DATE)),]


######################################
# 1- THE DATA -----
######################################
#We perform usual tests on the serie it self

pp.test(data[,oil_extrac])
adf.test(data[,oil_extrac])
kpss.test(data[,oil_extrac], null="Trend")

#The serie does not seem stationnary : we try first differance.

#We perform usual tests on the firt diff

pp.test(diff(data[,oil_extrac]))
adf.test(diff(data[,oil_extrac]))
kpss.test(diff(data[,oil_extrac]), null="Level")


#It is likely stationnary.


#Graph
ggplot(data, aes(x = DATE, y = oil_extrac)) +
  geom_line(color = "blue") +  
  labs(title = "Oil extraction",
       x = "Date",                                  
       y = "Oil extraction") +
  theme_minimal()   

diff_petrole=diff(data[,oil_extrac])
data[DATE>"1990-01-01", oil_extrac_diff:=diff_petrole]

ggplot(data, aes(x = DATE, y = oil_extrac_diff)) +
  geom_line(color = "blue") +  
  labs(title = "First difference of the oil extraction",
       x = "Date",                                  
       y = "FD Oil extraction") +
  theme_minimal()  


######################################
# 2- ARMA MODELS -----
######################################

#We select p and q using acf and pacf.

acf(diff(data[,oil_extrac]))
#q=4
pacf(diff(data[,oil_extrac]))
#p=4


for (i in 0:4) {
  sarima(data[,oil_extrac], 4,1,i, details= FALSE)
  sarima(data[,oil_extrac], 3,1,i, details= FALSE)
  sarima(data[,oil_extrac], 2,1,i, details= FALSE)
  sarima(data[,oil_extrac], 1,1,i, details= FALSE)
  sarima(data[,oil_extrac], 0,1,i, details= FALSE)
}


#We decide to reject the model if the p-values of some coefficients are higher than 10%. 
#Thus, we keep for now the following models : 

res20<-sarima(data[,oil_extrac], 2,1,0, details= FALSE)
res10<-sarima(data[,oil_extrac], 1,1,0, details= FALSE)
res43<-sarima(data[,oil_extrac], 4,1,3, details= FALSE)
res21<-sarima(data[,oil_extrac], 2,1,1, details= FALSE)
res32<-sarima(data[,oil_extrac], 3,1,2, details= FALSE)
res33<-sarima(data[,oil_extrac], 3,1,3, details= FALSE)

#exportation for the report
stargazer(res20$ttable)
stargazer(res10$ttable)
stargazer(res43$ttable)
stargazer(res21$ttable)
stargazer(res32$ttable)
stargazer(res33$ttable)

#Check that the residual are not correlated
print("ARIMA(2,1,0)") #REJECTED
LjungBox(res20$fit)
print("ARIMA(1,1,0)") #REJECTED
LjungBox(res10$fit)
print("ARIMA(4,1,3)") #ACCEPTED
LjungBox(res43$fit)
print("ARIMA(2,1,1)") #REJECTED
LjungBox(res21$fit)
print("ARIMA(3,1,2)") #ACEPTED
LjungBox(res32$fit)
print("ARIMA(3,1,3)") #REJECTED
LjungBox(res33$fit)

stargazer(LjungBox(res20$fit))
stargazer(LjungBox(res10$fit))
stargazer(LjungBox(res43$fit))
stargazer(LjungBox(res21$fit))
stargazer(LjungBox(res32$fit))
stargazer(LjungBox(res33$fit))


AIC(res32$fit)
AIC(res43$fit)

BIC(res32$fit)
BIC(res43$fit)


#########################
# 3- PREVISIONS ------
#########################
#ARIMA(4,1,3) 

serie=ts(data[,oil_extrac],
         start = c(1990, 1),
         end = c(2024, 3),
         frequency = 12)

prev=sarima.for(serie, p=4, d=1,q=3,n.ahead=2)


lower_b<- rbind(prev$pred[1] +1.96*prev$se[1],prev$pred[2] +1.96*prev$se[2])
upper_b<- rbind(prev$pred[1] -1.96*prev$se[1],prev$pred[2] -1.96*prev$se[2])
lower_b
upper_b

#On construit la matrice sigma avec les écart type et le coefficient de corrélation

sigma_x <- sqrt(res43$fit$sigma2)
sigma_y <- sqrt(res43$fit$sigma2*(1+(res43$fit$coef[1]+res43$fit$coef[5])^2))
cov <- res43$fit$sigma2*(res43$fit$coef[1]+res43$fit$coef[5])
Sigma <- rbind(c(sigma_x^2, cov), c(cov, sigma_y^2))
Sigma

plot(ellipse(Sigma, centre = prev$pred, level = 0.95), type = 'l', col = "blue", xlab = "Prediction at t+1", ylab = "Prediction at t+2", main = "95% Confidence Ellipse")
points(prev$pred[1], prev$pred[2], col = "red", pch = 19)
legend("topright", legend = c("95% Confidence Ellipse", "Last value obs"), col = c("blue", "red"), lwd = 2, pch = c(NA, 19))
