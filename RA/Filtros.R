# Exportación
Original$`FECHA DE NACIMIENTO` = gsub("/","-",format(as.Date(Original$`FECHA DE NACIMIENTO`),'%d/%m/%Y'))
Original$`FECHA DE LA ATENCIÓN` = gsub("/","-",format(as.Date(Original$`FECHA DE LA ATENCIÓN`),'%d/%m/%Y'))

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
Consulta_1 = Original[Original$NA_Documento %in% 1,]

Consulta_2 = Original[Original$NA_Documento %in% 1 & Original$Cruce %in% 1,]

Consulta_3 = Original[Original$NA_Documento %in% 1 & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1,]

Consulta_4 = Original[Original$NA_Documento %in% 1 & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1,]

Consulta_5 = Original[Original$NA_Documento %in% 1 & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1 & Original$List_Logros %in% 1,]

Consulta_6 = Consulta_2[!paste(Consulta_2$`ID OFERTA`, Consulta_2$`NUMERO DOCUMENTO`) %in% paste(Consulta_3$`ID OFERTA`, Consulta_3$`NUMERO DOCUMENTO`),]

nrow(Consulta_5[!duplicated(Consulta_5$A01),])

# Exportaciones
Campos = read_excel("~/GitHub/GIT-DPS/RA/1. Entradas/PlantillaRegistrosAdministrativos_20211008.xlsm", sheet = "Plantilla")
Campos = names(Campos)
Marcas = c("NA_Documento","Cruce","Dist_Nombres_Porc","Dist_Nombres_Dummy","Duplicados_Logro","List_Logros")

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

write.table(Original[c(Campos, Marcas)], file = paste("RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F)
# write.table(Consulta_5[c("A01","IdIntegrante", Campos, Marcas)], file = paste("RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F)


# write.table(Consulta_2[Campos], file = paste("Cargue_RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F)
# 
# Cargue_RA_12112021 = Cargue_RA_12112021[-1]
# 
# setnames(Cargue_RA_12112021, old = names(Cargue_RA_12112021), new = Campos)
# 
# Diferencia = Cargue_RA_16112021[!paste(Cargue_RA_16112021$`ID OFERTA`, Cargue_RA_16112021$`NUMERO DOCUMENTO`) %in%
#                                 paste(Cargue_RA_12112021$`ID OFERTA`, Cargue_RA_12112021$`NUMERO DOCUMENTO`),]
# 
# setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
# write.table(Diferencia, file = paste("Segundo_Cargue_RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F)
# 
# Id hogares unicos
# 
# setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
# write.table(Diferencia, file = paste("Segundo_Cargue_RA","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F)
# 
# # Diferencias de cargue
# setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
# 
# Cargue_RA_12112021 <- read_delim("Cargue_RA_12112021.txt", ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
# Campos = names(Cargue_RA_12112021)
# 
# 
