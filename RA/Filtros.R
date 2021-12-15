# Exportación
Campos = read_excel(paste(Entradas,"PlantillaRegistrosAdministrativos_20211008.xlsm", sep = slash), sheet = "Plantilla")
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

