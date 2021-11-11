# Reportes ####

# Conteos de NA por archivo de entrada RA
Consulta=(aggregate(Original, by=list(Original$Archivo), FUN=function(x) { sum(is.na(x))}))
Variables = c("ID OFERTA","TIPO DOCUMENTO","NUMERO DOCUMENTO","PRIMER NOMBRE","SEGUNDO NOMBRE","PRIMER APELLIDO","SEGUNDO APELLIDO","FECHA DE LA ATENCIÓN","ESTADO ACCESO A OFERTA")

Consulta = Consulta[c("Group.1",Variables)]
setnames(Consulta, old = "Group.1", new = "Archivo")

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.csv(Consulta, file =paste("NA","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento
Archivos_Cruce = DATA %>% group_by(Archivo) %>% summarise(Total=sum(Cruce, na.rm = T))
write.csv(Archivos_Cruce, file =paste("Archivos_Cruce","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento y oferta
Archivos_Cruce_Logros = Consulta %>% group_by(Archivo) %>% summarise(Total=sum(Cruce, na.rm = T))
write.csv(Archivos_Cruce_Logros, file =paste("Archivos_Cruce_Logros","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento, logros y oferta
Archivos_Cruce_Logros_Oferta = Consulta %>% group_by(Archivo) %>% summarise(Total=sum(Cruce, na.rm = T))
write.csv(Archivos_Cruce_Logros_Oferta, file =paste("Archivos_Cruce_Logros_Oferta","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# DATA = merge(DATA, Precargue[c("A01","IdIntegrante","E09","E01_1","E01_2","E01_3","E01_4","A03_1")], by.x = c("NUMERO DOCUMENTO","CODIGO MUNICIPIO DANE"), by.y = c("E09","A03_1"), all.x = T)
# Duplicados_DATA = Precargue[duplicated(Precargue$E09) | duplicated(Precargue$E09,fromLast = T),]
# Consulta$Cruce_duplicado = ifelse(paste(Consulta$`NUMERO DOCUMENTO`, Consulta$`CODIGO MUNICIPIO DANE`) %in% paste(Duplicados_Precargue$, Duplicados_Precargue$A03_1),1,0)

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

