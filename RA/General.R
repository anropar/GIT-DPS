# ===============================
#   VALIDACIONES Y CALCULOS ####
# ===============================
#'''En este código se encuentran los pasos para el cálculo de logros, IPM y LP de la Estrategia Unidos. Adicionalmente, contiene la 
# validación de la sabana de caracterización y la comparación entre los cálculos elaborados por el GIT y el Sistema de Información de 
# la Estrategia.'''

# Estos script tienen archivos de entrada que se deben controlar:

# 2.1. Importacion de datos.R (Path: Validaciones)
# Comparacion Logro.R (Path: Validaciones/3. Calculos)
# LOGRO27.R (Path: Validaciones/3. Calculos/Logros)
# LOGRO28.R (Path: Validaciones/3. Calculos/Logros)

Fecha = gsub("-","", Sys.Date())

Carpeta = dirname(rstudioapi::getSourceEditorContext()$path)#El ultimo slash o backslash no se debe incluir

slash = "/"

Entradas=paste(Carpeta,"1. Entradas", sep = slash)# Defina el escritorio de entrada donde están los archivos requeridos.
Salidas =paste(paste(Carpeta,"Salidas","Validacion_", sep = slash), Fecha, sep = "")# Defina el escritorio de salida donde serán enviado los archivos generados.

#     LIBRERIAS   ####
setwd(Carpeta)
source("Librerias.R", encoding = "UTF-8")# Las librerias que se usaran

###########################
# 1. Importación de datos #
###########################
setwd(Carpeta)
source("Importacion de datos.R", encoding = "UTF-8")

##############
# 2. Filtros #
##############
setwd(Carpeta)
source("Filtros.R", encoding = "UTF-8")

################
# 3. Consultas #
################
setwd(Carpeta)
source("Consultas.R", encoding = "UTF-8")

#####################
# 4. Exportaciones  #
#####################
Campos = read_excel(paste(Entradas,"PlantillaRegistrosAdministrativos_20211008.xlsm", sep = slash), sheet = "Plantilla")
Campos = names(Campos)

Entrega = "E3"

# Exportaciones
setwd(paste(Carpeta,"Salidas",Entrega, sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

# Original con marcas
Original$Exitosos = ifelse(!(Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1 & Original$Cruce_Oferta %in% 1 & Original$List_Logros %in% 1),1,0)
Original$Entrega = Entrega

write.table(Original[c(Campos, Marcas,"A01","IdIntegrante","Exitosos","Entrega")], file = paste("RA_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

# Consulta 5
# write.table(Consulta_5[Campos], file = paste("Consulta_5_OTI_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "UTF-8")
# write.table(Consulta_5[c(Campos,"A01","IdIntegrante")], file = paste("Consulta_5_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "UTF-8")

# Consulta 6
write.table(Consulta_6[Campos], file = paste("Consulta_6_OTI_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")
write.table(Consulta_6[c(Campos, Marcas,"A01","IdIntegrante")], file = paste("Consulta_6_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

# Archivos consulta
write.csv(Archivos_Consulta, file = paste("Archivos_Consulta_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)
# Archivos original
write.csv(Archivos_Original, file = paste("Archivos_Original_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

