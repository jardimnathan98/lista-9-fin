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

population<-read_excel("C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 9/pop brasil.xlsx")
population<-population[-(1:14),1:2]
datas_year<-as.character(seq(as.Date("1960/12/31"), by = "year", length.out = nrow(population)-1))
datas_year<-rev(datas_year)

population[-1,1]<-datas_year
colnames(population)<-c('periodo', 'populacao')

year2qquarter<-function(population ,inicial, final){
  datas_quarter<-as.character(seq( from= as.Date(inicial), by = "quarter", to=as.Date(final)))
  datas_quarter<-cbind(rev(datas_quarter),1)
j<-1
  for(i in 2:(nrow(population)-1)){
    datas_quarter[j,2]<-as.numeric(population[i,2]) -0*(as.numeric(population[i,2])-as.numeric(population[i+1,2]))/4
    datas_quarter[j+1,2]<-as.numeric(population[i,2]) -1*(as.numeric(population[i,2])-as.numeric(population[i+1,2]))/4
    datas_quarter[j+2,2]<-as.numeric(population[i,2]) -2*(as.numeric(population[i,2])-as.numeric(population[i+1,2]))/4
    datas_quarter[j+3,2]<-as.numeric(population[i,2]) -3*(as.numeric(population[i,2])-as.numeric(population[i+1,2]))/4
     j<-j+4
    }
return(datas_quarter)
}

inicial="1960/12/31"

final="2021-12-31"
pop_quarter<-year2qquarter(population, inicial, final)

consumo<-read_excel("C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 9/consumo real ajustado.xlsx")
consumo<-consumo[-(1:14),1:2]
datas_quart<-as.character(seq(as.Date("1996/03/31"), by = "quarter", length.out = nrow(consumo)-1))
datas_quart<-rev(datas_quart)

consumo[-1,1]<-datas_quart
colnames(consumo)<-c('periodo', 'consumo')
