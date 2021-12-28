setwd(Entradas)
BaseGestion_2021 = read_delim("BaseGestion Hogares Acompañados 2021.txt", ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

setwd(paste(Carpeta,"1. Entradas","Reporte_Administrativos", sep = slash))
ReporteAdministrativos = read_delim("ReporteAdministrativo_27_diciembre.txt", ";", escape_double = FALSE, trim_ws = TRUE)

Campos = read_excel(paste(Entradas,"PlantillaRegistrosAdministrativos_20211008.xlsm", sep = slash), sheet = "Plantilla")
Campos = names(Campos)

setwd(paste(Carpeta, "2. Salidas", sep = "/"))

RA_E1_02122021 = read_delim(paste("E1","RA_E1_02122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
RA_E2_02122021 = read_delim(paste("E2","RA_E2_16122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
RA_E3_23122021 = read_delim(paste("E3","RA_E3_23122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

Consulta_5_E1_02122021 = RA_E1_02122021[RA_E1_02122021$NA_Documento %in% 1 & (RA_E1_02122021$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & RA_E1_02122021$Cruce %in% 1 & RA_E1_02122021$Dist_Nombres_Dummy %in% 1 & RA_E1_02122021$Duplicados_Logro %in% 1 & RA_E1_02122021$Cruce_Oferta %in% 1 & RA_E1_02122021$List_Logros %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_6_E2_02122021 = RA_E2_02122021[RA_E2_02122021$NA_Documento %in% 1 & (RA_E2_02122021$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & RA_E2_02122021$Cruce %in% 1 & RA_E2_02122021$Dist_Nombres_Dummy %in% 1 & RA_E2_02122021$Duplicados_Logro %in% 1 & RA_E2_02122021$Cruce_Oferta %in% 1 & RA_E2_02122021$List_Logros %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_6_E2_02122021 = Consulta_6_E2_02122021[!paste(Consulta_6_E2_02122021$`NUMERO DOCUMENTO`, Consulta_6_E2_02122021$`ID OFERTA`, Consulta_6_E2_02122021$`ESTADO ACCESO A OFERTA`) %in% 
                                                paste(Consulta_5_E1_02122021$`NUMERO DOCUMENTO`, Consulta_5_E1_02122021$`ID OFERTA`, Consulta_5_E1_02122021$`ESTADO ACCESO A OFERTA`), ]

Consulta_6_E3_23122021 = RA_E3_23122021[RA_E3_23122021$NA_Documento %in% 1 & (RA_E3_23122021$`FECHA DE LA ATENCIÓN`)>="2021-01-01" & RA_E3_23122021$Cruce %in% 1 & RA_E3_23122021$Dist_Nombres_Dummy %in% 1 & RA_E3_23122021$Duplicados_Logro %in% 1 & RA_E3_23122021$Cruce_Oferta %in% 1 & RA_E3_23122021$List_Logros %in% 1,] %>% drop_na("NUMERO DOCUMENTO")
Consulta_6_E3_23122021 = Consulta_6_E3_23122021[(!paste(Consulta_6_E3_23122021$`NUMERO DOCUMENTO`, Consulta_6_E3_23122021$`ID OFERTA`, Consulta_6_E3_23122021$`ESTADO ACCESO A OFERTA`) %in% 
                                                 paste(Consulta_5_E1_02122021$`NUMERO DOCUMENTO`, Consulta_5_E1_02122021$`ID OFERTA`, Consulta_5_E1_02122021$`ESTADO ACCESO A OFERTA`)), ]
Consulta_6_E3_23122021 = Consulta_6_E3_23122021[(!paste(Consulta_6_E3_23122021$`NUMERO DOCUMENTO`, Consulta_6_E3_23122021$`ID OFERTA`, Consulta_6_E3_23122021$`ESTADO ACCESO A OFERTA`) %in% 
                                                   paste(Consulta_6_E2_02122021$`NUMERO DOCUMENTO`, Consulta_6_E2_02122021$`ID OFERTA`, Consulta_6_E2_02122021$`ESTADO ACCESO A OFERTA`)), ]

# Asigna una compilación de las columnas comunes entre los archivos de consulta 5 y 6 de las entregas
Interseccion = intersect(names(Consulta_5_E1_02122021), intersect(names(Consulta_6_E2_02122021), names(Consulta_6_E3_23122021)))

# Con las columnas comunas compila en un solo dataframe con rbind
Consulta_6_AC = rbind(Consulta_5_E1_02122021[Interseccion], Consulta_6_E2_02122021[Interseccion], Consulta_6_E3_23122021[Interseccion])
Consulta_6_AC$Cruce_RA = ifelse(Consulta_6_AC$IdIntegrante %in% ReporteAdministrativos$Id_Persona, 1, 0)
# Trae el campo Estadohogar del archivo BaseGestion
Consulta_6_AC = merge(Consulta_6_AC, BaseGestion_2021[c("idIntegranteHogar","EstadoHogar")], by.x = "IdIntegrante", by.y = "idIntegranteHogar", all.x = T)

setwd(paste(Carpeta,"2. Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.table(Consulta_6_AC[Consulta_6_AC$Cruce_RA %in% 0, c(Campos,"EstadoHogar")], file = paste("Consulta_6_AC","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

nrow(Consulta_5_E1_02122021[!duplicated(Consulta_5_E1_02122021$A01),])
nrow(Consulta_6_E2_02122021[!duplicated(Consulta_6_E2_02122021$A01),])
nrow(Consulta_6_E3_23122021[!duplicated(Consulta_6_E3_23122021$A01),])
nrow(Consulta_6_AC[!duplicated(Consulta_6_AC$A01),])

nrow(ReporteAdministrativos[!duplicated(ReporteAdministrativos$Id_Hogar),])
