ibc <- function(){

library(readxl)

ibc <- read_excel("C:/Users/analista04/Desktop/App/ibc.xlsx",  col_types = c("date", "numeric", "numeric",  "numeric"))
ibc=as.data.frame(ibc)
serie_ibc_1=xts(ibc[,3], as.Date(ibc$fecha))
serie_ibc_2=xts(ibc[,2], as.Date(ibc$fecha))

grafico_ibc_1=hchart(serie_ibc_1, name = "% Var")
grafico_ibc_2=hchart(serie_ibc_2, name = "Puntos")

datos=list("grafico_1"=grafico_ibc_1,"grafico_2"=grafico_ibc_2)

return(datos)

}