# Exportación
Campos = read_excel("~/GitHub/GIT-DPS/RA/1. Entradas/PlantillaRegistrosAdministrativos_20211008.xlsm", sheet = "Plantilla")
Campos = names(Campos)
Marcas = c("NA_Documento","Cruce","Dist_Nombres_Porc","Dist_Nombres_Dummy","Duplicados_Logro","Cruce_Oferta","List_Logros")

Original$`TIPO DOCUMENTO` = as.character(recode_factor(Original$`TIPO DOCUMENTO`, `1` = "Registro Civil",
                                                         `2` = "Tarjeta de Identidad", `3` = "Cédula de Ciudadanía",
                                                         `4` = "Cédula de Extranjería", `5` = "Documento Nacional de Identidad (DNI) del país de origen",
                                                         `6` = "Pasaporte", `7` = "Salvoconducto para refugiado",
                                                         `8` = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos"))

# Filtro de los registros adminitrativos por las reglas:
# 1. Cruzar el Numero de documento con la base precargue.
# 2. No estar duplicado por numero de documento y logro.
# 3. Contener los logros definidos en la compilación patterns.

# Consulta ####
Consulta_1 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01",] %>% drop_na("NUMERO DOCUMENTO")
Consulta_2 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_3 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_4 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1 & Original$Cruce_Oferta %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_5 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1 & Original$Cruce_Oferta %in% 1 & Original$List_Logros %in% 1,] %>% drop_na("NUMERO DOCUMENTO")

# Id hogar unicos de consulta 5
nrow(Consulta_5[!duplicated(Consulta_5$A01),])

#
# Consulta_5_P1 = read_delim("~/GitHub/GIT-DPS/RA/2. Sabana/Salidas/E1/Consulta_5_E1_30112021.txt", ";", escape_double = FALSE, trim_ws = TRUE)
# 
# Consulta_5$ACTIVIDAD_PRODUCTIVA = ifelse(grepl("ACTIVIDAD PRODUCTIVA", toupper(Consulta_5$`LOGRO y/o PRIVACIÓN GESTIONADA`)),1,0)
# Consulta_5_P1$ACTIVIDAD_PRODUCTIVA = ifelse(grepl("ACTIVIDAD PRODUCTIVA", toupper(Consulta_5_P1$`LOGRO y/o PRIVACIÓN GESTIONADA`)),1,0)
# 
# Consulta_5[Consulta_5$ACTIVIDAD_PRODUCTIVA %in% 1,]
# Consulta_5_P1[Consulta_5_P1$ACTIVIDAD_PRODUCTIVA %in% 1,]
# 
# Prueba = merge(Consulta_5[Consulta_5$ACTIVIDAD_PRODUCTIVA %in% 1,], Consulta_5_P1[Consulta_5_P1$ACTIVIDAD_PRODUCTIVA %in% 1,], by=c("NUMERO DOCUMENTO", "ESTADO ACCESO A OFERTA"), all.x=T)
# 
# Consulta_5$Reportado = ifelse(Consulta_5[Consulta_5$ACTIVIDAD_PRODUCTIVA %in% 1,]$`NUMERO DOCUMENTO` %in% 
#                               Consulta_5_P1[Consulta_5_P1$ACTIVIDAD_PRODUCTIVA %in% 1,]$`NUMERO DOCUMENTO` &
#                               (as.character(Consulta_5$`ESTADO ACCESO A OFERTA`) %in% as.character(Consulta_5_P1$`ESTADO ACCESO A OFERTA`)),1,0)
# 
# Consulta_6 = Consulta_5[Consulta_5$Reportado %in% 0,]
# 
# Consulta_6$`FECHA DE NACIMIENTO` = gsub("/","-",format(as.Date(Consulta_6$`FECHA DE NACIMIENTO`),'%d/%m/%Y'))
# Consulta_6$`FECHA DE LA ATENCIÓN` = gsub("/","-",format(as.Date(Consulta_6$`FECHA DE LA ATENCIÓN`),'%d/%m/%Y'))

setwd(Carpeta)
source("Consultas.R")


Consulta_5 = Consulta_5 %>% drop_na(`NUMERO DOCUMENTO`) %>% arrange(`ID OFERTA`)

Entrega = "E1"

# Exportaciones
setwd(paste(Carpeta,"2. Sabana","Salidas",Entrega, sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

# Original con marcas
write.table(Original[c(Campos, Marcas,"A01","IdIntegrante")], file = paste("RA_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

# Consulta 5
write.table(Consulta_5[c(Campos, Marcas,"A01","IdIntegrante")], file = paste("Consulta_5_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "UTF-8")

# Consulta 6
write.table(Consulta_6[c(Campos, Marcas,"A01","IdIntegrante")], file = paste("Consulta_6_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

# Archivos consulta
write.csv(Archivos_Consulta, file = paste("Archivos_Consulta_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)
# Archivos original
write.csv(Archivos_Original, file = paste("Archivos_Original_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)


# Consulta 2020
write.table(Consulta_5_E1[year(Consulta_5_E1$`FECHA DE LA ATENCIÓN`) %in% "2020",], file = paste("Consulta_5_E1_","2020","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

RA_E1_30112021 = RA_E1_30112021[RA_E1_30112021$NA_Documento %in% 1 & (RA_E1_30112021$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & RA_E1_30112021$Cruce %in% 1 & RA_E1_30112021$Dist_Nombres_Dummy %in% 1 & RA_E1_30112021$Duplicados_Logro %in% 1 & RA_E1_30112021$Cruce_Oferta %in% 1 & RA_E1_30112021$List_Logros %in% 1,]
