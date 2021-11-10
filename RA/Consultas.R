
Consulta = merge(Base_Gestion_2021, DATA[c("IdIntegrante","E01_1","E01_2","E01_3","E01_4","E08","E09","E02")], by.x = "idIntegranteHogar", by.y = "IdIntegrante")

Consulta=(aggregate(DATA, by=list(DATA$Archivo), FUN=function(x) { sum(is.na(x))}))
Variables = c("ID OFERTA","TIPO DOCUMENTO","NUMERO DOCUMENTO","PRIMER NOMBRE","SEGUNDO NOMBRE","PRIMER APELLIDO","SEGUNDO APELLIDO","FECHA DE LA ATENCIÓN","LOGRO y/o PRIVACIÓN GESTIONADA")

Consulta = Consulta[c("Group.1",Variables)]

setnames(Consulta, old = "Group.1", new = "Archivo")

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

write.csv(Consulta, file =paste("NA","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)