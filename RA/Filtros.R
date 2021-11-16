# Distancias de nombres ####
Prueba = DATA[DATA$Cruce %in% 1,]

# Prueba = Consulta_1[!paste(Consulta_1$`TIPO DOCUMENTO`, Consulta_1$`NUMERO DOCUMENTO`) %in%
#                       paste(Consulta_2$`TIPO DOCUMENTO`, Consulta_2$`NUMERO DOCUMENTO`),]

Prueba = merge(Prueba, Precargue[c("A01","IdIntegrante","E08","E09","E01_1","E01_2","E01_3","E01_4","A03_1")], by.x = c("NUMERO DOCUMENTO"), by.y = c("E09"), all.x = T)

setwd("~/GitHub/GIT-DPS/Consultas")
source("Limpiador de textos.R")

Prueba$Nombres_RA = paste(Prueba$`PRIMER NOMBRE`, limpiador_texto(Prueba$`PRIMER APELLIDO`))
Prueba$Nombres_PR = paste(Prueba$E01_1, Prueba$E01_3)

Prueba$Dist_Nombres = mapply(adist, limpiador_texto(Prueba$Nombres_RA), limpiador_texto(Prueba$Nombres_PR))
Prueba$Dist_Nombres_Porc = (Prueba$Dist_Nombres/nchar(Prueba$Nombres_RA))*100

Consulta = Prueba[!Prueba$Dist_Nombres_Porc>=50,]# Conservo los registros con un porcentaje de distancia inferior al
# 50 porciento.

# setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
# write.csv(Prueba[Prueba$Dist_Nombres_Porc>=50,], file =paste("Diferencias_Campos_Basicos","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Filtro de los registros adminitrativos por las reglas:
# 1. Cruzar el Numero de documento con la base precargue.
# 2. No estar duplicado por numero de documento y logro.
# 3. Contener los logros definidos en la compilación patterns.

Consulta_1=Consulta[Consulta$Cruce %in% 1 & Consulta$Duplicados_Logro %in% 0,]

patterns <- c("ACTIVIDAD PRODUCTIVA", "AFILIACIÓN A SALUD", "EDUCACIÓN INICIAL",
              "ESCOLARIZACIÓN","NO TRABAJO INFANTIL","ACCESO A AGUA","SANEAMIENTO BÁSICO",
              "LEER Y ESCRIBIR","ESTUDIOS POSTSECUNDARIOS", "NO PISOS EN TIERRA",
              "PAREDES ADECUADAS","NO HACINAMIENTO","ACTIVIDAD PRODUCTIVA","FAMILIAS EN ACCIÓN")

# Consulta ####
Consulta_2=Consulta[Consulta$Cruce %in% 1 & Consulta$Duplicados_Logro %in% 0 & 
                        grepl(paste(patterns, collapse="|"), toupper(Consulta$`LOGRO y/o PRIVACIÓN GESTIONADA`)),]

Consulta_3 = Consulta_1[!paste(Consulta_1$`ID OFERTA`, Consulta_1$`NUMERO DOCUMENTO`) %in% paste(Consulta_2$`ID OFERTA`, Consulta_2$`NUMERO DOCUMENTO`),]

Consulta_4 = Consulta[Consulta$Cruce %in% 1 & Consulta$Duplicados_Logro %in% 1,]


Consulta_2$`FECHA DE NACIMIENTO` = gsub("/","-",format(as.Date(Consulta_2$`FECHA DE NACIMIENTO`),'%d/%m/%Y'))
Consulta_2$`FECHA DE LA ATENCIÓN` = gsub("/","-",format(as.Date(Consulta_2$`FECHA DE LA ATENCIÓN`),'%d/%m/%Y'))

Consulta_2$`TIPO DOCUMENTO` = as.character(recode_factor(Consulta_2$`TIPO DOCUMENTO`, `1` = "Registro Civil",
                                                   `2` = "Tarjeta de Identidad", `3` = "Cédula de Ciudadanía",
                                                   `4` = "Cédula de Extranjería", `5` = "Documento Nacional de Identidad (DNI) del país de origen",
                                                   `6` = "Pasaporte", `7` = "Salvoconducto para refugiado",
                                                   `8` = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos"))

Campos <- read_excel("~/GitHub/GIT-DPS/RA/1. Entradas/PlantillaRegistrosAdministrativos_20211008.xlsm", sheet = "Plantilla")
Campos = names(Campos)

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.table(Consulta_2[Campos], file = paste("Cargue_RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE)
