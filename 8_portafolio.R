portafolio_bnc <- read_excel("C:/Users/analista04/Desktop/App/portafolio.xlsx", 
                         sheet = "BNC", col_types = c("date", 
                                                      "numeric", "numeric", "numeric"))
portafolio_bnc=as.data.frame(portafolio_bnc)

bnc=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","bnc.txt",sep=""),"BNC")

portafolio_bnc[,5]=portafolio_bnc[,2]*bnc$precio
portafolio_bnc[,6]=((portafolio_bnc[,5]-portafolio_bnc[,4])/portafolio_bnc[,4])*100

colnames(portafolio_bnc)<-c("Fecha","Cantidad","Precio de Adquisición", "Valor de Adquisición", "Valor Actual", "Rendimiento")

#########################################################################################################################
portafolio_tdvd <- read_excel("C:/Users/analista04/Desktop/App/portafolio.xlsx", 
                             sheet = "TDV.D", col_types = c("date", 
                                                          "numeric", "numeric", "numeric"))

portafolio_tdvd=as.data.frame(portafolio_tdvd)

tdvd=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","tdv.d.txt",sep=""),"TDV.D")

portafolio_tdvd[,5]=portafolio_tdvd[,2]*tdvd$precio
portafolio_tdvd[,6]=((portafolio_tdvd[,5]-portafolio_tdvd[,4])/portafolio_tdvd[,4])*100

colnames(portafolio_tdvd)<-c("Fecha","Cantidad","Precio de Adquisición", "Valor de Adquisición", "Valor Actual", "Rendimiento")

#########################################################################################################################

portafolio_mvza <- read_excel("C:/Users/analista04/Desktop/App/portafolio.xlsx", 
                              sheet = "MVZ.A", col_types = c("date", 
                                                             "numeric", "numeric", "numeric"))

portafolio_mvza=as.data.frame(portafolio_mvza)

mvza=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","mvz.a.txt",sep=""),"MVZ.A")

portafolio_mvza[,5]=portafolio_mvza[,2]*mvza$precio
portafolio_mvza[,6]=((portafolio_mvza[,5]-portafolio_mvza[,4])/portafolio_mvza[,4])*100

colnames(portafolio_mvza)<-c("Fecha","Cantidad","Precio de Adquisición", "Valor de Adquisición", "Valor Actual", "Rendimiento")

#########################################################################################################################

portafolio_mvzb <- read_excel("C:/Users/analista04/Desktop/App/portafolio.xlsx", 
                              sheet = "MVZ.B", col_types = c("date", 
                                                             "numeric", "numeric", "numeric"))

portafolio_mvzb=as.data.frame(portafolio_mvzb)

mvzb=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","mvz.b.txt",sep=""),"MVZ.B")

portafolio_mvzb[,5]=portafolio_mvzb[,2]*mvzb$precio
portafolio_mvzb[,6]=((portafolio_mvzb[,5]-portafolio_mvzb[,4])/portafolio_mvzb[,4])*100

colnames(portafolio_mvzb)<-c("Fecha","Cantidad","Precio de Adquisición", "Valor de Adquisición", "Valor Actual", "Rendimiento")

#########################################################################################################################


portafolio_bvcc <- read_excel("C:/Users/analista04/Desktop/App/portafolio.xlsx", 
                              sheet = "BVCC", col_types = c("date", 
                                                             "numeric", "numeric", "numeric"))

portafolio_bvcc=as.data.frame(portafolio_bvcc)

bvcc=datos_accion(accion,paste("C:/Users/analista04/Desktop/App/","bvcc.txt",sep=""),"BVCC")

portafolio_bvcc[,5]=portafolio_bvcc[,2]*bvcc$precio
portafolio_bvcc[,6]=((portafolio_bvcc[,5]-portafolio_bvcc[,4])/portafolio_bvcc[,4])*100

colnames(portafolio_bvcc)<-c("Fecha","Cantidad","Precio de Adquisición", "Valor de Adquisición", "Valor Actual", "Rendimiento")


#########################################################################################################################
