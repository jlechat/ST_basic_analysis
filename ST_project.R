#Linear Time Series Assignment :
#ARIMA modelling of a time series

library(insee)
library(data.table)
library(tidyverse)
library(zoo)
library(tseries)

#0- DATA IMPORTATION AND FORMATING ----

data = as.data.table(insee::get_insee_idbank("010767584"))
