############
#VALIDACION#
############

#Versión de diccionario de datos: "DiccionarioDatos_FormularioCaracterizacion_2019_V6"
setwd(paste(Carpeta,"1. Entradas", sep = slash))

Campos = read_excel("PlantillaRegistrosAdministrativos_20211008.xlsm", sheet = "Plantilla")
Campos = names(Campos)

setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Nombres de campos.R")# Contiene las variables definidas en el diccionario de datos correspondiente

setwd(paste(Carpeta,"2. Sabana","Salidas",paste0("Validacion_",Fecha), sep = slash))
dir.create(paste0("Etapa_1_",Fecha),showWarnings = FALSE)#Crea una nueva carpeta

n<-max(length(Error_Nombres_variables_2),length(Error_Nombres_variables_1))
length(Error_Nombres_variables_1)<-n

Error_Nombres=as.data.frame(cbind(Error_Nombres_variables_1, Error_Nombres_variables_2))

setnames(Error_Nombres, old = c("Error_Nombres_variables_1","Error_Nombres_variables_2"), 
                        new = c("Diff_Diccionario","Diff_Sabana"), skip_absent = T)

rm(Error_Nombres_variables_1,Error_Nombres_variables_2)

setwd(paste(Carpeta,"2. Sabana","Salidas",paste0("Validacion_",Fecha),paste0("Etapa_1_",Fecha), sep = slash))
write.csv(Error_Nombres, file = paste("Error_Nombres","_",format(Sys.time(), "%d%m%Y"), ".csv", sep=""), row.names = FALSE, fileEncoding = "UTF-8")

rm(list = ls()[!ls() %in% grep("^DATA|Precargue|Oferta|Error|Resumen|Version|Salidas|Entradas|Carpeta|General|Fecha|slash",ls(),value = TRUE)])
