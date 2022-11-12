library(lubridate)
library(quantmod)
library(dplyr) 
library(PerformanceAnalytics)
library(ggplot2)
library(tidyverse)
library(writexl)
library(readxl)
library(vrtest)
library(xlsx)
# define os ativos que ir?o ser coletados
# define a data de in?cio da coleta
start <- "2012-09-30"
end<-"2022-07-01"
getSymbols("^BVSP",
           warnings = FALSE,
           from = start,
           to = end,
           src = "yahoo")

bvsp_returns_quart <- Ad(BVSP) %>%
  to.quarterly(indexAt = "lastof",
               OHLC = FALSE) %>%
  Return.calculate(method = "log") %>% #log do preço final, menos o log do preço atual
  na.omit() %>%
  `colnames<-`("ibovespa")

bvsp_returns_quart<-as.matrix(bvsp_returns_quart)

load("C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 9/ipca_quarter.Rdata")
#multiplico por 4 para anualizar

bvsp_returns_quart_real<-  ((1+ bvsp_returns_quart[,1])/(1 +as.numeric(ipca_quarter[,2])))^4 -1
# eleva por 4 para anualizar

bvsp_returns_quart_real<-as.matrix(bvsp_returns_quart_real)

#


# pegar di 1 mes  para livre de risco
di<-read.csv("C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 9/di futuro.csv")
di_1m<-di[,2:3]
date<-as.matrix(rev(di_1m[1:119,1]))#botar na mesma data dos demais
value<-as.matrix(rev(di_1m[1:119,2]))
di_1m<-cbind(date, value)


ipca <-read_excel("C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/ipca.xlsx")
ipca=t(ipca)
ipca<-(ipca[-(1:2),])
datas_month<-as.character(seq(as.Date("1994/07/01"), by = "month", length.out = nrow(ipca)))
ipca<-cbind(datas_month, ipca)
colnames(ipca)<-c('datas', "ipca mensal", ' semanas por mes')
ipca_mes<-(ipca[223:nrow(ipca),1:2])
#######

di_real<-(1+as.numeric(di_1m[1:nrow(ipca_mes),2])/100)^(1/12)/(1+as.numeric(ipca_mes[,2])/100) -1


di_real<-cbind(di_1m[1:nrow(ipca_mes),1], di_real)



x<-row.names(bvsp_returns_quart_real)
di_quarter<-cbind(x,0)#formar di trimestral
k<-1
j<-1
  while (k < nrow(di_real)) {
    di_quarter[j,2]<-((1 +as.numeric(di_real[k,2]))*(1 +as.numeric(di_real[k+1,2]))*
      (1 +as.numeric(di_real[k+2,2])))^4 -1
    
    
    j<-j+1 
    k<-k+3
  }
  

########## consumo

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
######### criar consumo per capita
consumo<-consumo[-(1:3),]
pop_quarter<-pop_quarter[1:nrow(consumo),]
consumo$pop<-pop_quarter[,2]
consumo$cons_per_cap<-as.numeric(consumo$consumo)/as.numeric(consumo$pop)

delta_cons<-seq(1,nrow(consumo) -1)

for( i in 1:(nrow(consumo) -1) ){
  delta_cons[i]<- log(consumo$cons_per_cap[i]) -log(consumo$cons_per_cap[i+1])
  
}

consumo$delta<-c(delta_cons, NA)
mean(consumo$delta[1:40])


mean(bvsp_returns_quart_real)

mean(as.numeric(di_quarter[,2]))



sd(consumo$delta[1:40])# sd para consumo, rf e bsp

sd(bvsp_returns_quart_real)

sd(as.numeric(di_quarter[,2]))
acf(consumo$delta[1:40], lag=1,pl=FALSE)# acf com lag 1 para os 3 tambem
####################
#tabela 2

# aer = mean(bvsp_returns_quart_real) - mean(di_quarter[,3]) + 1/2 sd(bvsp_returns_quart_real)
  
# sd(er) = sd(bvsp) pois variancia do fixo é zero e nao tem covariancia com retorno fixo

# dividir coluna 1 da 2

# correlacao de bovespa com variacao do consumo

#rra(1) =  aer dividido por covariancia entre delta consumo e bovespa

# rra(2) = rra(1) vezes correlação do bovespa com variacao do consumo

# tabela 3
# para trp  


