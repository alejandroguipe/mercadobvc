datos_inflacion <-function(directorio_archivo_1,directorio_archivo_2){
  
  library(dygraphs)
  library(xts)
  library(readr)
  library(readxl)
  library(zoo)
  library(dplyr)
  library(magrittr)
  library(highcharter)
  
  inflacion_mensual= read_excel(directorio_archivo_1, col_types = c("date", "numeric"))
  inflacion_diaria = read_excel(directorio_archivo_2, col_types = c("date", "numeric"))
  
  
  #Construccion de serie de inflacion mensual
  serie_inflacion_mensual=xts(inflacion_mensual[,2], inflacion_mensual$Fecha)
  
  #Construccion de serie de inflacion diaria
  serie_inflacion_diaria=xts(inflacion_diaria[,2], inflacion_diaria$Fecha)
  
  #Highchart 
  grafico_inflacion_mensual=hchart(serie_inflacion_mensual)
  
  
  #Dygraph
  #grafico_inflacion_mensual=dygraph(serie_inflacion_mensual,main="Inflación Mensual") %% 
  #dyAxis(y, label = Bs.) %%
  #dyRangeSelector() 
  
  return(grafico_inflacion_mensual)
  
}