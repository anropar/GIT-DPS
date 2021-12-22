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
# 1. Excluir regristros sin número de cédula por fuera de la fecha.
# 2. Cruzar el Numero de documento con la base precargue.
# 3. Distancia de los nombres inferior al 50%.
# 4. No estar duplicado por numero de documento y logro.
# 5. Contener los logros definidos en la compilación patterns.

# Consulta ####
# No olvidar ejecutar en generación
Consulta_1 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01",] %>% drop_na("NUMERO DOCUMENTO")
Consulta_2 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_3 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_4 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1 & Original$Cruce_Oferta %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_5 = Original[Original$NA_Documento %in% 1 & (Original$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & Original$Cruce %in% 1 & Original$Dist_Nombres_Dummy %in% 1 & Original$Duplicados_Logro %in% 1 & Original$Cruce_Oferta %in% 1 & Original$List_Logros %in% 1,] %>% drop_na("NUMERO DOCUMENTO")

# Id hogar unicos de consulta 5
nrow(Consulta_5[!duplicated(Consulta_5$A01),])

Entrega = "E2"

Consulta_5_P1 = read_delim(paste(Carpeta,"2. Sabana","Salidas","E1", "Consulta_5_E1_02122021.txt", sep = slash), ";", escape_double = FALSE, trim_ws = TRUE)
Consulta_6_P2 = read_delim(paste(Carpeta,"2. Sabana","Salidas",Entrega, "Consulta_6_E2_16122021.txt", sep = slash), ";", escape_double = FALSE, trim_ws = TRUE)

Consulta_AC = rbind(Consulta_5_P1[intersect(names(Consulta_5_P1), names(Consulta_6_P2))], 
                    Consulta_6_P2[intersect(names(Consulta_5_P1), names(Consulta_6_P2))])# Para las entregas en tiempos posteriores deben acumularsen. Aquí debería estar el rbind

Consulta_5$Reportado = ifelse(paste(Consulta_5$`NUMERO DOCUMENTO`, Consulta_5$`ID OFERTA`, Consulta_5$`ESTADO ACCESO A OFERTA`) %in%
                              paste(Consulta_AC$`NUMERO DOCUMENTO`, Consulta_AC$`ID OFERTA`, Consulta_AC$`ESTADO ACCESO A OFERTA`),1,0)

Consulta_6 = Consulta_5[Consulta_5$Reportado %in% 0,]

Consulta_6$`FECHA DE NACIMIENTO` = gsub("/","-",format(as.Date(Consulta_6$`FECHA DE NACIMIENTO`),'%d/%m/%Y'))
Consulta_6$`FECHA DE LA ATENCIÓN` = gsub("/","-",format(as.Date(Consulta_6$`FECHA DE LA ATENCIÓN`),'%d/%m/%Y'))

# Id hogar unicos de consulta 6
nrow(Consulta_6[!duplicated(Consulta_6$A01),])

# Aporte a la meta de acompañados

