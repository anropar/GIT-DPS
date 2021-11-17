# 
patterns = c("ACTIVIDAD PRODUCTIVA", "AFILIACIÓN A SALUD", "EDUCACIÓN INICIAL",
             "ESCOLARIZACIÓN","NO TRABAJO INFANTIL","ACCESO A AGUA","SANEAMIENTO BÁSICO",
             "LEER Y ESCRIBIR","ESTUDIOS POSTSECUNDARIOS", "NO PISOS EN TIERRA",
             "PAREDES ADECUADAS","NO HACINAMIENTO","ACTIVIDAD PRODUCTIVA","FAMILIAS EN ACCIÓN")

DATA$List_Logros = ifelse(grepl(paste(patterns, collapse="|"), toupper(DATA$`LOGRO y/o PRIVACIÓN GESTIONADA`)),1,0)

Consulta = DATA[DATA$Cruce %in% 1,]

# Distancias de nombres ####
Consulta = merge(Consulta, Precargue[c("A01","IdIntegrante","E08","E09","E01_1","E01_2","E01_3","E01_4","A03_1")], by.x = c("NUMERO DOCUMENTO"), by.y = c("E09"), all.x = T)

setwd("~/GitHub/GIT-DPS/Consultas")
source("Limpiador de textos.R")

Consulta$Nombres_RA = paste(Consulta$`PRIMER NOMBRE`, limpiador_texto(Consulta$`PRIMER APELLIDO`))
Consulta$Nombres_PR = paste(Consulta$E01_1, Consulta$E01_3)

Consulta$Dist_Nombres = mapply(adist, limpiador_texto(Consulta$Nombres_RA), limpiador_texto(Consulta$Nombres_PR))
Consulta$Dist_Nombres_Porc = (Consulta$Dist_Nombres/nchar(Consulta$Nombres_RA))*100
Consulta$Dist_Nombres_Dummy = ifelse(!Consulta$Dist_Nombres_Porc>=50,1,0)

# Consulta = Consulta[Consulta$Dist_Nombres_Dummy==1,]# Conservo los registros con un porcentaje de distancia inferior al
# 50 porciento.

# setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
# write.csv(Consulta[Consulta$Dist_Nombres_Porc>=50,], file =paste("Diferencias_Campos_Basicos","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Filtro de los registros adminitrativos por las reglas:
# 1. Cruzar el Numero de documento con la base precargue.
# 2. No estar duplicado por numero de documento y logro.
# 3. Contener los logros definidos en la compilación patterns.

library("dplyr", lib.loc="~/R/R-3.6.3/library")
#Se excluyen duplicados por numero de documento y ID conservando el valor con la menor proporción de diferencias de nombres
Consulta = Consulta %>% group_by(`NUMERO DOCUMENTO`, ID) %>% slice(which.min(Dist_Nombres_Porc))

# Consulta ####
Consulta_1 = Consulta[Consulta$Cruce %in% 1 & Consulta$Dist_Nombres_Dummy %in% 1  &  Consulta$Duplicados_Logro %in% 1,]

Consulta_2 = Consulta[Consulta$Cruce %in% 1 & Consulta$Dist_Nombres_Dummy %in% 1 & Consulta$Duplicados_Logro %in% 1 & Consulta$List_Logros==1,]

Consulta_3 = Consulta_1[!paste(Consulta_1$`ID OFERTA`, Consulta_1$`NUMERO DOCUMENTO`) %in% paste(Consulta_2$`ID OFERTA`, Consulta_2$`NUMERO DOCUMENTO`),]

Consulta_4 = Consulta[Consulta$Cruce %in% 1 & Consulta$Duplicados_Logro %in% 1,]

# Exportación
Consulta_2$`FECHA DE NACIMIENTO` = gsub("/","-",format(as.Date(Consulta_2$`FECHA DE NACIMIENTO`),'%d/%m/%Y'))
Consulta_2$`FECHA DE LA ATENCIÓN` = gsub("/","-",format(as.Date(Consulta_2$`FECHA DE LA ATENCIÓN`),'%d/%m/%Y'))

Consulta_2$`TIPO DOCUMENTO` = as.character(recode_factor(Consulta_2$`TIPO DOCUMENTO`, `1` = "Registro Civil",
                                                   `2` = "Tarjeta de Identidad", `3` = "Cédula de Ciudadanía",
                                                   `4` = "Cédula de Extranjería", `5` = "Documento Nacional de Identidad (DNI) del país de origen",
                                                   `6` = "Pasaporte", `7` = "Salvoconducto para refugiado",
                                                   `8` = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos"))

Campos = read_excel("~/GitHub/GIT-DPS/RA/1. Entradas/PlantillaRegistrosAdministrativos_20211008.xlsm", sheet = "Plantilla")
Campos = names(Campos)

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.table(Consulta_2[Campos], file = paste("Cargue_RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F)

# Diferencias de cargue
Cargue_RA_12112021 <- read_delim("Cargue_RA_12112021.txt", ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
Campos = names(Cargue_RA_12112021[-16])

Cargue_RA_12112021 = Cargue_RA_12112021[-1]

setnames(Cargue_RA_12112021, old = names(Cargue_RA_12112021), new = Campos)

Diferencia = Cargue_RA_16112021[!paste(Cargue_RA_16112021$`ID OFERTA`, Cargue_RA_16112021$`NUMERO DOCUMENTO`) %in%
                                paste(Cargue_RA_12112021$`ID OFERTA`, Cargue_RA_12112021$`NUMERO DOCUMENTO`),]

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.table(Diferencia, file = paste("Segundo_Cargue_RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F)

# Id hogares unicos
nrow(Consulta_2[!duplicated(Consulta_2$A01),])

Marcas = c("Cruce","Dist_Nombres_Porc","Dist_Nombres_Dummy","Duplicados_Logro","List_Logros")

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.table(Diferencia, file = paste("Segundo_Cargue_RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F)
