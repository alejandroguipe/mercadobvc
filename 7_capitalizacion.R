capitalizacion <-function(){
  
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
  
  datos_accion <-function(accion, directorio, nombre){
    
    library(dygraphs)
    library(xts)
    library(readr)
    library(readxl)
    library(zoo)
    library(dplyr)
    library(magrittr)
    library(highcharter)
    
    
    #abca=datos_accion(abca,paste(directorio,"abc.a.txt",sep=""),"ABC.A") C:/Users/analista04/Desktop/App/ 
    #"C:/Users/analista04/Desktop/App/abc.a.txt"
    #AL ACTUALIZAR ESTA FUNCION ACTUALIZAR EN EL HTML
    
    
    accion <- read_delim(directorio, 
                         "|", escape_double = FALSE, col_names = FALSE, 
                         col_types = cols(X1 = col_skip(), X11 = col_skip(), 
                                          X2 = col_datetime(format = "%d/%m/%Y ")), 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE)
    
    
    accion=as.data.frame(accion)
    colnames(accion)<-c("Fecha","Tipo.de.operacion",paste(nombre,".Open",sep=""),paste(nombre,".Close",sep=""),
                        paste(nombre,".High",sep=""),paste(nombre,".Low",sep=""),
                        ".Cantidad.de.Operaciones",paste(nombre,".Volume",sep=""),".Monto.Efectivo")
    
    accion=accion[c("Fecha", paste(nombre,".Open",sep=""), paste(nombre,".High",sep=""), 
                    paste(nombre,".Low",sep=""), paste(nombre,".Close",sep=""), paste(nombre,".Volume",sep=""), 
                    ".Cantidad.de.Operaciones", ".Monto.Efectivo", "Tipo.de.operacion") ]
    ##REVISADO
    
    fecha_reconversion=as.Date("2018-08-18", format="%Y-%m-%d")
    accion=subset(accion, 
                  accion$Tipo.de.operacion=="R"&(accion[,3]>0&accion[,4]>0))
    
    if (nombre=="BVCC")
    {
      accion[-c(1,2,3,4),]
    }
    
    accion_nominal=accion
    i=1
    
    while (accion[i,1]<=fecha_reconversion)
    {
      accion[i,2]=accion[i,2]/100000
      accion[i,3]=accion[i,3]/100000
      accion[i,4]=accion[i,4]/100000
      accion[i,5]=accion[i,5]/100000
      accion[i,8]=accion[i,8]/100000
      i=i+1
    }
    
    rm(i)
    
    serie_accion=xts(accion[,2:8], accion$Fecha)
    
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
    
    serie_dolar_paralelo=xts(dolarparalelo[,3], as.Date(dolarparalelo$Fecha))
    
    #REVISADO##############################################################################################################
    
    inflacion_diaria <- read_excel("C:/Users/analista04/Desktop/App/3_inflacion_diaria.xlsx", 
                                   col_types = c("date", "numeric"))
    inflacion_diaria=as.data.frame(inflacion_diaria)
    
    fechas_cotizaciones=as.Date(intersect(as.Date(dolarparalelo$Fecha),as.Date(accion$Fecha)))
    
    fechas_cotizaciones_inflacion=as.Date(intersect(as.Date(inflacion_diaria$Fecha),as.Date(accion$Fecha)))
    
    accion_usd=accion_nominal[as.Date(accion_nominal$Fecha) %in% fechas_cotizaciones, ]
    
    accion_inflacion_bs=accion[as.Date(accion$Fecha) %in% fechas_cotizaciones_inflacion, ]
    
    
    for (i in 1:nrow(as.data.frame(intersect(as.Date(dolarparalelo$Fecha),as.Date(accion_nominal$Fecha))))) {
      
      s=accion_usd[accion_usd$Fecha==fechas_cotizaciones[i],2:7]
      r=as.numeric(dolarparalelo[as.Date(dolarparalelo$Fecha)==fechas_cotizaciones[i],2])
      
      accion_usd[accion_usd$Fecha==fechas_cotizaciones[i],2:7]=s/r
      
    }
    
    serie_accion_usd=xts(accion_usd[,2:6], accion_usd$Fecha)
    
    x=accion_inflacion_bs[1,5]
    for (i in 1:nrow(as.data.frame(accion_inflacion_bs[,5])))
    {
      
      accion_inflacion_bs[,5]=x*(1+as.numeric(accion_inflacion_bs[i,5]))
    } 
    
    serie_accion_inflacion_bs=xts(accion_inflacion_bs[,2:6], as.Date(accion_inflacion_bs$Fecha))
    
    #Highchart Bs.S
    grafico_bs=highchart(type = "stock") %>% 
      hc_yAxis_multiples(create_yaxis(2, height = c(3, 1), turnopposite = TRUE)) %>% 
      hc_add_series(serie_accion[,1:4], yAxis = 0, name = nombre) %>% 
      hc_add_series(serie_accion[,5], color = "gray", yAxis = 1, name = "Volumen", type = "column")  
    
    #Highchart USD
    grafico_usd=highchart(type = "stock") %>%  
      hc_yAxis_multiples(create_yaxis(2, height = c(3, 1), turnopposite = TRUE)) %>% 
      hc_add_series(serie_accion_usd[,1:4], yAxis = 0, name = nombre) %>%  
      hc_add_series(serie_accion[,5], color = "gray", yAxis = 1, name = "Volumen", type = "column")  
    
    #Highchart Inflación
    grafico_inflacion=hchart(serie_accion_inflacion_bs)
    
    #Dygraph Bs.
    #grafico_bs=dygraph(serie_accion[2:5], main=nombre) %>% dyCandlestick() %>% 
    #dyAxis("y", label = "Bs.") %>%
    #dyRangeSelector() 
    
    
    #Dygraph USD
    #grafico_usd=dygraph(serie_accion_usd[2:5],main=nombre) %>% dyCandlestick() %>% 
    #dyAxis("y", label = "USD") %>%
    #dyRangeSelector() 
    
    
    precio_actual=accion[nrow(accion),5]
    
    datos <- list("grafico_bs" = grafico_bs, "grafico_usd"=grafico_usd,"accion_usd"=accion_usd,
                  "accion_bs"=accion, "precio"=precio_actual,"grafico_inflacion"=grafico_inflacion)
    
    return(datos)
    
  }
  
  ################################################################################################################
  
  abca=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","abc.a.txt",sep=""),"ABC.A")
  bnc=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","bnc.txt",sep=""),"BNC")
  bou=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","bou.txt",sep=""),"BOU")
  bpv=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","bpv.txt",sep=""),"BPV")
  bvcc=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","bvcc.txt",sep=""),"BVCC")
  bvl=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","bvl.txt",sep=""),"BVL")
  ccr=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","ccr.txt",sep=""),"CCR")
  cgq=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","cgq.txt",sep=""),"CGQ")
  cie=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","cie.txt",sep=""),"CIE")
  crma=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","crm.a.txt",sep=""),"CRM.A")
  dom=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","dom.txt",sep=""),"DOM")
  efe=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","efe.txt",sep=""),"EFE")
  env=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","env.txt",sep=""),"ENV")
  fnc=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","fnc.txt",sep=""),"FNC")
  fvia=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","fvi.a.txt",sep=""),"FVI.A")
  fvib=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","fvi.b.txt",sep=""),"FVI.B")
  gzl=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","gzl.txt",sep=""),"GZL")
  ivc=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","ivc.txt",sep=""),"IVC")
  mpa=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","mpa.txt",sep=""),"MPA")
  mvza=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","mvz.a.txt",sep=""),"MVZ.A")
  mvzb=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","mvz.b.txt",sep=""),"MVZ.B")
  pgr=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","pgr.txt",sep=""),"PGR")
  ptn=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","ptn.txt",sep=""),"PTN")
  rst=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","rst.txt",sep=""),"RST")
  svs=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","svs.txt",sep=""),"SVS")
  tdvd=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","tdv.d.txt",sep=""),"TDV.D")
  tpg=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","tpg.txt",sep=""),"TPG")
  ################################################################################################################
  
  acciones_en_cirulacion=data.frame(
    acciones = c ("ABC.A","BNC","BOU","BPV","BVCC","BVL","CCR","CGQ","CIE","CRM.A","DOM", "EFE","ENV","FNC", 
                  "FVI.A", "FVI.B","GZL","IVC","MPA","MVZ.A","MVZ.B","PGR","PTN","RST","SVS","TDV.D","TPG"),
    
    numero_de_acciones=c( 89166667,
                          5791930372,
                          23796748346,
                          107827475,
                          29250000,
                          3647133702,
                          2834081,
                          91196788,
                          485560500,
                          150000000,
                          24062500,
                          700000000,
                          126923808,
                          81081000,
                          1730129521,
                          10615414833,
                          24251124,
                          111025339,
                          229410000,
                          60880929,
                          43880032,
                          732317516,
                          152986904,
                          285642388,
                          52524376,
                          26121595,
                          1188344665),
    
    
    precio=c(abca$precio, bnc$precio, bou$precio, bpv$precio, bvcc$precio, bvl$precio, ccr$precio, 
             cgq$precio, cie$precio, crma$precio, dom$precio, efe$precio, env$precio, fnc$precio, 
             fvia$precio, fvib$precio, gzl$precio, ivc$precio, mpa$precio, mvza$precio, mvzb$precio, pgr$precio,
             ptn$precio, rst$precio, svs$precio, tdvd$precio, tpg$precio)
    
    
  
  )
}

dolar=datos_dolar()
precio_dolar=dolar$precio
acciones_en_cirulacion[,4]=acciones_en_cirulacion[,2]*acciones_en_cirulacion[,3]
acciones_en_cirulacion[,5]=acciones_en_cirulacion[,4]/precio_dolar
acciones[,6]=diario_acciones[,3]/dolar$precio
colnames(acciones_en_cirulacion)=c("Acción","Número de acciones","Precio Bs.","Capitalización Bs","Capitalización $USD", "Precio $USD")


capitalizacion_total=kable(acciones_en_cirulacion) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

return(capitalizacion_total)
