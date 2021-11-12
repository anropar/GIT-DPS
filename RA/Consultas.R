# Reportes ####

# Conteos de NA por archivo de entrada RA
Consulta=(aggregate(Original, by=list(Original$Archivo), FUN=function(x) { sum(is.na(x))}))
Variables = c("ID OFERTA","TIPO DOCUMENTO","NUMERO DOCUMENTO","PRIMER NOMBRE","SEGUNDO NOMBRE","PRIMER APELLIDO","SEGUNDO APELLIDO","FECHA DE LA ATENCIÓN","ESTADO ACCESO A OFERTA")

Consulta = Consulta[c("Group.1",Variables)]
setnames(Consulta, old = "Group.1", new = "Archivo")

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.csv(Consulta, file =paste("NA","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento en dataframe Original
Archivos_Original = Original %>% group_by(Archivo) %>% summarise(Total=n())
write.csv(Archivos_Original, file =paste("Archivos_Original","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento en dataframe DATA
Archivos_DATA = Original %>% group_by(Archivo) %>% summarise(Total=n())
write.csv(Archivos_DATA, file =paste("Archivos_DATA","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento en dataframe DATA
Archivos_Cruce = DATA %>% group_by(Archivo) %>% summarise(Total=sum(Cruce, na.rm = T))
write.csv(Archivos_Cruce, file =paste("Archivos_Cruce","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento en dataframe Consulta
Archivos_Consulta_1 = Consulta_1 %>% group_by(Archivo) %>% summarise(Total=n())
write.csv(Archivos_Consulta_1, file =paste("Archivos_Consulta_1","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento y oferta en dataframe Consulta
Archivos_Cruce_Logros = Consulta %>% group_by(Archivo) %>% summarise(Total=sum(Cruce, na.rm = T))
write.csv(Archivos_Cruce_Logros, file =paste("Archivos_Cruce_Logros","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Conteos por archivo de los que cruzaron por documento, logros y oferta
Archivos_Cruce_Logros_Oferta = Consulta %>% group_by(Archivo) %>% summarise(Total=sum(Cruce, na.rm = T))
write.csv(Archivos_Cruce_Logros_Oferta, file =paste("Archivos_Cruce_Logros_Oferta","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# DATA$`FECHA DE NACIMIENTO` = gsub("/","-",format(as.Date(DATA$`FECHA DE NACIMIENTO`),'%d/%m/%Y'))
# DATA$`FECHA DE LA ATENCIÓN` = gsub("/","-",format(as.Date(DATA$`FECHA DE LA ATENCIÓN`),'%d/%m/%Y'))


# DATA = merge(DATA, Precargue[c("A01","IdIntegrante","E09","E01_1","E01_2","E01_3","E01_4","A03_1")], by.x = c("NUMERO DOCUMENTO","CODIGO MUNICIPIO DANE"), by.y = c("E09","A03_1"), all.x = T)
# Duplicados_DATA = Precargue[duplicated(Precargue$E09) | duplicated(Precargue$E09,fromLast = T),]
# Consulta$Cruce_duplicado = ifelse(paste(Consulta$`NUMERO DOCUMENTO`, Consulta$`CODIGO MUNICIPIO DANE`) %in% paste(Duplicados_Precargue$, Duplicados_Precargue$A03_1),1,0)
