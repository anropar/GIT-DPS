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
General =paste(Carpeta,"General", sep = slash)# Defina el escritorio donde se encuentra el script con el nombre de "General.R"

#     LIBRERIAS   ####
setwd(Carpeta)
source("Librerias.R", encoding = "UTF-8")# Las librerias que se usaran

###########################
# 1. Importación de datos #
###########################
setwd(Carpeta)
source("Importacion de datos.R", encoding = "UTF-8")

#############
# 2. Consultas #
#############
setwd(Carpeta)
source("Filtros.R", encoding = "UTF-8")

setwd(Carpeta)
source(paste(Carpeta,"Consultas", sep = slash), encoding = "UTF-8")

#####################
# 7. Exportaciones  #
#####################
Entrega = "E1"

# Exportaciones
setwd(paste(Carpeta,"2. Sabana","Salidas",Entrega, sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

# Original con marcas
Original$Exitosos = ifelse(!(Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1 & Original$Cruce_Oferta %in% 1 & Original$List_Logros %in% 1),1,0)
Original$Entrega = Entrega

Prueba = merge(Original, ReporteAdministrativos[c("ID Oferta","Id_Persona","ID Registro Administrativo")], by.x = c("ID OFERTA","IdIntegrante"), by.y = c("ID Oferta","Id_Persona"), all.x = T)
setnames(Prueba, old = c("ID OFERTA","ID Registro Administrativo"), new = c("ID_OFERTA","ID_Registro_Administrativo"))

Prueba =  Prueba[!is.na(Prueba$ID_Registro_Administrativo),] %>% group_by(ID_OFERTA, IdIntegrante) %>%
                          mutate(lab = toString(ID_Registro_Administrativo)) %>%
                          as.data.frame()

Prueba = Prueba[!duplicated(Prueba$lab),]

write.table(Original[c(Campos, Marcas,"A01","IdIntegrante","Exitosos","Entrega")], file = paste("RA_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

# Consulta 5
write.table(Consulta_5[Campos], file = paste("Consulta_5_OTI_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "UTF-8")
write.table(Consulta_5[c(Campos,"A01","IdIntegrante")], file = paste("Consulta_5_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "UTF-8")

# Consulta 6
write.table(Consulta_6[c(Campos, Marcas,"A01","IdIntegrante")], file = paste("Consulta_6_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

# Archivos consulta
write.csv(Archivos_Consulta, file = paste("Archivos_Consulta_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)
# Archivos original
write.csv(Archivos_Original, file = paste("Archivos_Original_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Consulta 2020
write.table(Consulta_5_E1[year(Consulta_5_E1$`FECHA DE LA ATENCIÓN`) %in% "2020",], file = paste("Consulta_5_E1_","2020","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")
RA_E1_30112021 = RA_E1_30112021[RA_E1_30112021$NA_Documento %in% 1 & (RA_E1_30112021$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & RA_E1_30112021$Cruce %in% 1 & RA_E1_30112021$Dist_Nombres_Dummy %in% 1 & RA_E1_30112021$Duplicados_Logro %in% 1 & RA_E1_30112021$Cruce_Oferta %in% 1 & RA_E1_30112021$List_Logros %in% 1,]
Data_2020=Original[Original$NA_Documento %in% 1 & !(Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1 & Original$Cruce_Oferta %in% 1 & Original$List_Logros %in% 1,] %>% drop_na("NUMERO DOCUMENTO")

#############
# 2. Marcas #
#############
DATA = Precargue

setwd(paste(Carpeta,"Consultas", sep = slash))
source("Perfiles.R", encoding = "UTF-8")
source("Ciclo vital.R", encoding = "UTF-8")
source("Mujer jefe.R", encoding = "UTF-8")

DATA = Perfiles(DATA, A01, E02_1)# Genera la columna de perfil del hogar
DATA = Ciclo(DATA, E02_1)# Genera la columna de ciclo vital
DATA = Mujer_jefe(DATA, E03, E14)# Genera la columna de ciclo vital

###############
# 3. Victimas #
###############
setwd(Carpeta)
source("Victimas.R", encoding = "UTF-8")# Genera las marcas de victimas

###############################
# 10. Frecuencias municipales #
###############################
setwd(Carpeta)
source("Frecuencias municipales.R", encoding = "UTF-8")# Genera el archivo de frecuencias municipales
