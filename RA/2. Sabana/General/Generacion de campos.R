# Generación de campos
detach("package:dplyr", unload=TRUE)
library("dplyr", lib.loc="~/R/R-3.6.3/library")

Original$NA_Documento = ifelse(!is.na(Original$`NUMERO DOCUMENTO`),1,0)

Original$Cruce = ifelse(Original$`NUMERO DOCUMENTO` %in% Precargue$E09,1,0)

Original$Duplicados_Logro = ifelse(duplicated(paste(Original$`NUMERO DOCUMENTO`, toupper(Original$`LOGRO y/o PRIVACIÓN GESTIONADA`), Original$`ESTADO ACCESO A OFERTA`)),0,1)

Original$Cruce_Oferta = ifelse(paste(Original$`ID OFERTA`, Original$`CODIGO MUNICIPIO DANE`) %in% paste(Oferta$`ID Oferta`, Oferta$`Cód Municipio`),1,0)

Original = merge(Original, Precargue[c("A01","IdIntegrante","A02","A02_1","A03","A03_1","E02","E03","E08","E09","E01_1","E01_2","E01_3","E01_4")], by.x = c("NUMERO DOCUMENTO"), by.y = c("E09"), all.x = T)

source(paste(Carpeta,"Consultas", sep = slash), encoding = "UTF-8")
source("Limpiador de textos.R")

Original$Nombres_RA = paste(Original$`PRIMER NOMBRE`, limpiador_texto(Original$`PRIMER APELLIDO`))
Original$Nombres_PR = paste(Original$E01_1, Original$E01_3)

Original$Dist_Nombres = mapply(adist, limpiador_texto(Original$Nombres_RA), limpiador_texto(Original$Nombres_PR))
Original$Dist_Nombres_Porc = (Original$Dist_Nombres/nchar(Original$Nombres_RA))*100
Original$Dist_Nombres_Dummy = ifelse(!Original$Dist_Nombres_Porc>=50,1,0)

library("dplyr", lib.loc="~/R/R-3.6.3/library")
#Se excluyen duplicados por numero de documento y ID conservando el valor con la menor proporción de diferencias de nombres
Original = Original %>% group_by(`NUMERO DOCUMENTO`, ID) %>% slice(which.min(Dist_Nombres_Porc))# Conserva los registros que tienen el nombre con menor distancia

# Original$Cruce_TIPO_NUMERO = ifelse(paste(Original$`TIPO DOCUMENTO`, Original$`NUMERO DOCUMENTO`) %in% paste(Precargue$E08, as.character(Precargue$E09)),1,0)
# Original$Duplicados = ifelse(duplicated(Original$`NUMERO DOCUMENTO`),1,0)
# Original$Cruce_Precargue = ifelse(paste(Original$`ID OFERTA`, Original$`CODIGO MUNICIPIO DANE`) %in% paste(Oferta$`ID Oferta`, Oferta$`Cód Municipio`),1,0)

