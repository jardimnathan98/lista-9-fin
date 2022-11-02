library(lubridate)
library(quantmod)
library(dplyr) 
library(PerformanceAnalytics)
library(ggplot2)
library(tidyverse)
library(writexl)
library(readxl)
library(vrtest)
library(GetTddata)
library(xlsx)
# define os ativos que ir?o ser coletados
# define a data de in?cio da coleta

start <- "2012-06-30"
getSymbols("^BVSP",
           warnings = FALSE,
           from = start,
           src = "yahoo")
#fazer retornos trimestrais aqui

#

di<-read.csv("C:/Users/Nathan/Downloads/livros economia/financas/finanças puc/lista 9/di futuro.csv")
#pegar di 1 mes a cada 3 meses para livre de risco
