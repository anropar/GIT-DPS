
# Filtro de los registros adminitrativos por las reglas:
# 1. Cruzar el Numero de documento con la base precargue.
# 2. No estar duplicado por numero de documento y logro.
# 3. Contener los logros definidos en la compilación patterns.

patterns <- c("ACTIVIDAD PRODUCTIVA", "AFILIACIÓN A SALUD", "EDUCACIÓN INICIAL",
              "ESCOLARIDAD","NO TRABAJO INFANTIL","ACCESO A AGUA","SANEAMIENTO BÁSICO",
              "LEER Y ESCRIBIR","ESTUDIOS POSTSECUNDARIOS", "NO PISOS EN TIERRA",
              "PAREDES ADECUADAS","NO HACINAMIENTO","ACTIVIDAD PRODUCTIVA","FAMILIAS EN ACCIÓN")

# Consulta ####
Consulta=DATA[DATA$Cruce %in% 1 & DATA$Duplicados_Logro %in% 0 & 
                grepl(paste(patterns, collapse="|"), toupper(DATA$`LOGRO y/o PRIVACIÓN GESTIONADA`)),]

# Nombres ####
Prueba = DATA[DATA$Cruce %in% 1,]
Prueba = merge(Prueba, Precargue[c("A01","IdIntegrante","E09","E01_1","E01_2","E01_3","E01_4","A03_1")], by.x = c("NUMERO DOCUMENTO"), by.y = c("E09"), all.x = T)

setwd("~/GitHub/GIT-DPS/Consultas")
source("Limpiador de textos.R")

Prueba$Nombres_RA = paste(toupper(Prueba$`PRIMER NOMBRE`), toupper(Prueba$`PRIMER APELLIDO`))
Prueba$Nombres_PR = paste(Prueba$E01_1, Prueba$E01_3)

Prueba$Dist_Nombres = mapply(adist,Prueba$Nombres_RA, Prueba$Nombres_PR)
Prueba$Dist_Nombres_Porc = (Prueba$Dist_Nombres/nchar(Prueba$Nombres_RA))*100

# Consulta=Consulta[!paste(Consulta$`NUMERO DOCUMENTO`,Consulta$`LOGRO y/o PRIVACIÓN GESTIONADA`) %in% paste(Primer_Cargue_Registros_Administrativos$`NUMERO DOCUMENTO`, Primer_Cargue_Registros_Administrativos$`LOGRO y/o PRIVACIÓN GESTIONADA`),]
# Consulta=DATA[DATA$Cruce %in% 1,]
# Consulta=DATA[DATA$Cruce %in% 1 & DATA$Duplicados_Logro %in% 0,]

# Duplicados ####

Duplicados_Precargue = Precargue[duplicated(Precargue$E09) | duplicated(Precargue$E09, fromLast = T),c("A01","IdIntegrante","E08","E09","E01_1","E01_2","E01_3","E01_4","A03_1")]

Duplicados_Consulta = Consulta[duplicated(paste(Consulta$`NUMERO DOCUMENTO`, Consulta$`CODIGO MUNICIPIO DANE`)) |
                                 duplicated(paste(Consulta$`NUMERO DOCUMENTO`, Consulta$`CODIGO MUNICIPIO DANE`), fromLast = T),]

DATA$Dist_Nombres = mapply(adist, DATA$Nombre_total_1, DATA$Nombre_total_2)

# 
Consulta$`TIPO DOCUMENTO` = as.numeric(as.character(recode_factor(Consulta$`TIPO DOCUMENTO`, `Registro Civil` = 1,
                                                                  `Tarjeta de Identidad` = 2, `Cédula de Ciudadanía` = 3,
                                                                  `Cédula de Extranjería` = 4, `Documento Nacional de Identidad (DNI) del país de origen` = 5,
                                                                  `Pasaporte` = 6,
                                                                  `Salvoconducto para refugiado` = 7,
                                                                  `Permiso especial de permanencia (PEP) para ciudadanos venezolanos` = 8)))

Consulta = merge(Consulta, Precargue[!duplicated(Precargue$E09),c("A01","IdIntegrante","E08","E09","E01_1","E01_2","E01_3","E01_4","A03_1")], by.x = c("NUMERO DOCUMENTO"), by.y = c("E09"), all.x = T)

nrow(Consulta[!duplicated(Consulta$A01),])
nrow(Consulta[!duplicated(Consulta$IdIntegrante),])

Consulta = Consulta %>% group_by(A01) %>% mutate(Total_Personas = n())

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.csv(table(Consulta$`LOGRO y/o PRIVACIÓN GESTIONADA`), file =paste("Consulta_Logros","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)


