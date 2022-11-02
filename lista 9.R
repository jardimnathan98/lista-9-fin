library(lubridate)
library(quantmod)
library(dplyr) 
library(PerformanceAnalytics)
library(ggplot2)
library(tidyverse)
#library(writexl)
#library(readxl)
#library(vrtest)
#library(GetTddata)
#library(xlsx)
# define os ativos que irao ser coletados
# define a data de inicio da coleta

start <- "2012-06-30"
getSymbols("^BVSP",
           warnings = FALSE,
           from = start,
           src = "yahoo")

#fazer retornos trimestrais aqui
bvsp_adj<-as.data.frame(BVSP$BVSP.Adjusted)
bvsp_adj_returns<-log()


bvsp_returns_quart <- Ad(BVSP) %>%
  to.quarterly(indexAt = "lastof",
               OHLC = FALSE) %>%
  Return.calculate(method = "log") %>%
  na.omit() %>%
  `colnames<-`("ibovespa")

bvsp_returns_quart<-as.matrix(bvsp_returns_quart)
load("/home/nathan/Downloads/mestrado/lista 9/ipca_quarter.Rdata")

bvsp_returns_quart_real<-bvsp_returns_quart[1:(nrow(bvsp_returns_quart)- 3),1]/(1 + as.numeric(ipca_quarter[75:(nrow(ipca_quarter)-1),2])/100)
bvsp_returns_quart_real<-as.matrix(bvsp_returns_quart_real)
#
#pegar di 1 mes a cada 3 meses para livre de risco
di<-read.csv("/home/nathan/Downloads/mestrado/lista 9/di futuro.csv")
di_1m<-di[,2:3]
di_1m<-di_1m<-as.matrix(rev(di_1m[1:119,]))#botar na mesma data dos demais

di_quarter<-cbind(bvsp_returns_quart_real,0)
j<-1
for(i in 0:(nrow(di_1m)/3)){
  di_quarter[j,]<-di_1m[nrow(di_1m)-3*i,]
j<-j+1
  }


#montar consumo per capita

