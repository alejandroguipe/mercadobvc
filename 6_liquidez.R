m2 <- function(){

library(readxl)
liquidez <- read_excel("C:/Users/analista04/Desktop/App/liquidez.xlsx", 
                       col_types = c("date", "numeric", "numeric"))
liquidez=as.data.frame(liquidez)
liquidez=liquidez[2:nrow(liquidez),]

library(dygraphs)
library(xts)
library(readr)
library(readxl)
library(zoo)
library(dplyr)
library(magrittr)
library(highcharter)

m2=liquidez[,2]
serie_m2=xts(liquidez[,3], as.Date(liquidez$fecha))

grafico_m2=hchart(serie_m2, name = "% Var. M2")

datos=list("grafico"=grafico_m2,"liquidez_m2"=m2,"serie_m2"=serie_m2)


return(datos)

}