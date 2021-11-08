###########################
#   Validacion mensual    #
#                         #
#   Andres Romero Parra   #
#                         #
###########################
setwd("~/Datos/Revisión consistencia logros/Distancia entre nombres")
NoDOCUMENTOS <- read.csv("~/Datos/Revisión consistencia logros/Distancia entre nombres/Novigentes.csv", sep=";")
vigentes   <- read.csv("~/Datos/Revisión consistencia logros/Distancia entre nombres/vigentes.csv", sep=";")
DOCUMENTOS <- read_excel("~/Datos/Revisión consistencia logros/Distancia entre nombres/CurceRegistraduriaBDUnidosSinTipoDocumento.xlsx", skip = 1)

colnames(DOCUMENTOS)[c(11:12)]                     <- c("IDHOGAR","ID_INTEGRANTE")

attach(DOCUMENTOS)
DOCUMENTOS$Nombre_total_1<-paste(PrimerNombre,SegundoNombre,PrimerApellido,SegundoApellido)
DOCUMENTOS$Nombre_total_2<-paste(primernombre,segundoNombre,primerApellido,segundoApellido)
DOCUMENTOS$Nombre_total_1<-gsub("NULL","",DOCUMENTOS$Nombre_total_1)
DOCUMENTOS$Nombre_total_2<-gsub("NULL","",DOCUMENTOS$Nombre_total_2)
DOCUMENTOS$Error_T       <-mapply(adist,DOCUMENTOS$Nombre_total_1,DOCUMENTOS$Nombre_total_2)
DOCUMENTOS$Error_T_por   <-100*DOCUMENTOS$Error_T/(nchar(DOCUMENTOS$Nombre_total_1))
DOCUMENTOS$Error_T_por_cat  <-cut(DOCUMENTOS$Error_T_por, breaks = c(-0.9,10,20,40,60,80,210), right = T)
DOCUMENTOS$Error_T_por_cat  <-recode_factor(DOCUMENTOS$Error_T_por_cat, `(-0.9,10]` = "0-10", `(10,20]` = "11-20", `(20,40]` = "21-40", `(40,60]` = "41-60", `(60,80]` = "60-80", `(80,210]` = "Más de 80")
DOCUMENTOS$Caracteres_1     <-nchar(DOCUMENTOS$Nombre_total_1)
DOCUMENTOS$Caracteres_2     <-nchar(DOCUMENTOS$Nombre_total_2)

DOCUMENTOS$Error_PN_por<-(DOCUMENTOS$Error_PN/nchar(DOCUMENTOS$primernombre))*100
DOCUMENTOS$Error_SN_por<-(DOCUMENTOS$Error_SN/nchar(DOCUMENTOS$segundoNombre))*100
DOCUMENTOS$Error_PA_por<-(DOCUMENTOS$Error_PA/nchar(DOCUMENTOS$primerApellido))*100
DOCUMENTOS$Error_SA_por<-(DOCUMENTOS$Error_SA/nchar(DOCUMENTOS$segundoApellido))*100
DOCUMENTOS$Error_T_por <-100*DOCUMENTOS$Error_T/(nchar(DOCUMENTOS$primernombre)+nchar(DOCUMENTOS$segundoNombre)+nchar(DOCUMENTOS$primerApellido)+nchar(DOCUMENTOS$segundoApellido))

View(DOCUMENTOS[DOCUMENTOS$Error_T_por>40,c(1:2,11:12,23:24,7,16,25:29)])

setwd("~/Datos/Revisión consistencia logros/Distancia entre nombres")
write.csv2(table(DOCUMENTOS$Error_T), file = "Frec_Error_T_19092018.csv", row.names = FALSE)
write.csv2(DOCUMENTOS[DOCUMENTOS$Error_T_por>40,c(1:2,11:12,23:24,7,16,25:29)], file = "Error_MAS40_19092018.csv", row.names = FALSE)
write.csv2(DOCUMENTOS[DOCUMENTOS$Error_T_por<=40,c(1:2,11:12,23:24,7,16,25:29)], file = "Error_MENOS41_19092018.csv", row.names = FALSE)

