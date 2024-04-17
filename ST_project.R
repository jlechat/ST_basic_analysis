#Linear Time Series Assignment :
#ARIMA modelling of a time series

library(insee)
library(data.table)
library(tidyverse)
library(zoo)
library(tseries)
library(fUnitRoots)

#0- DATA IMPORTATION AND FORMATING ----

data = as.data.table(insee::get_insee_idbank("010767584"))

data=data[,c("DATE", "OBS_VALUE")]
setnames(data, c("DATE", "extraction_gaz_naturel"))

data<-data[order(as.Date(data$DATE)),]


xm.source<-zoo(data[[2]])
xm<-xm.source[1:length(xm.source)]


#1- PART 1-----
plot(xm, xaxt="n")
axis(side=1,at=seq(0,412,12))


#Decreasing trend
summary(lm(extraction_gaz_naturel~DATE, data=data))

#yes and an intercept


#Auto correlation tests

adf<-adfTest(data[,extraction_gaz_naturel], lag=0, type="ct")

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}


Qtests(adf@test$lm$residuals,24, fitdf=length(adf@test$lm$coefficients))
#It invalidates the test without lag

adfTest_valid<-function(series,kmax,type){ 
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <- Qtests(adf@test$lm$residuals,24,fitdf=length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T) == 0) {
      noautocorr <- 1; cat("OK \n")}
    else cat("nope \n")
    k <- k + 1
  }
  return(adf)
}
adf <- adfTest_valid(data[,extraction_gaz_naturel],24,"ct")

#21 lags
adf<-adfTest(data[,extraction_gaz_naturel], lag=21, type="ct")
adf

# p_value>>> 5% : we can't reject the existence of a unit root
#our serie is at leat l(1)

#we diff the serie
extraction_gaz_naturel_diff=diff(data[,extraction_gaz_naturel])
extraction_gaz_naturel_diff<-c(NA,extraction_gaz_naturel_diff)
data[, extraction_gaz_naturel_diff:=extraction_gaz_naturel_diff]

plot(data[,extraction_gaz_naturel_diff])

summary(lm(extraction_gaz_naturel_diff~DATE, data = data))
#probably no trend

adf<-adfTest_valid(data[,extraction_gaz_naturel_diff], 24, type="nc")

#we reject the unit root hypothesis
#extraction_gaz_naturel is l(1)


#2- PART 2 -----
x<-na.remove(extraction_gaz_naturel_diff)
par(mfrow=c(1,2))
acf(x);pacf(x)
