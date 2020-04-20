datos_dolar <- function(){
  
  library(dygraphs)
  library(xts)
  library(readr)
  library(readxl)
  library(zoo)
  library(dplyr)
  library(magrittr)
  library(highcharter)
  
  url <- "https://dxj1e0bbbefdtsyig.woldrssl.net/custom/dolartoday.xlsx"#
  destfile <- "dolartoday.xlsx" #
  curl::curl_download(url, destfile)#
  dolartoday <- read_excel(destfile, col_types = c("text","numeric", "numeric", "text", "numeric","numeric"))#
  
  dolarparalelo=as.data.frame(dolartoday[,1:3]) #
  
  colnames(dolarparalelo)<-c("Fecha","Bs.","Bs.S") #
  
  dolarparalelo[,1]=as.Date(dolarparalelo[,1], format="%m-%d-%Y") #
  
  #dolarparalelo <- read_excel(directorio, col_types = c("date", "numeric", "numeric"))
  
  dolarparalelo[1:2813,3]=dolarparalelo[1:2813,2]/100000
  dolarparalelo[2814:nrow(dolarparalelo),3]=dolarparalelo[2814:nrow(dolarparalelo),2]
  dolar_paralelo=dolarparalelo[,3]
  
  serie_dolar_paralelo=xts(dolarparalelo[,3], as.Date(dolarparalelo$Fecha))
  
  #Dygraph
  #grafico_dolar_paralelo=dygraph(serie_dolar_paralelo,main="Dolar Paralelo") %% 
  #dyAxis(y, label = USDVES) %%
  #dyRangeSelector(dateWindow = c(2019-01-01, 2019-10-01)) 
  
  #Highchart Bs.S
  grafico_dolar_paralelo=hchart(serie_dolar_paralelo, name = "USD/Bs.")
  
  
  precio_actual=dolar_paralelo[nrow(as.data.frame(dolar_paralelo))]
  
  datos=list("grafico"=grafico_dolar_paralelo,"dolar"=dolar_paralelo,"serie"=serie_dolar_paralelo, 
             "precio"=precio_actual)
  
  return(datos)
  
}