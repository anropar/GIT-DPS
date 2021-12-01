# Reportes ####

# Conteos de NA por archivo de entrada RA
Consulta=(aggregate(Original, by=list(Original$Archivo), FUN=function(x) { sum(is.na(x))}))
Variables = c("ID OFERTA","TIPO DOCUMENTO","NUMERO DOCUMENTO","PRIMER NOMBRE","SEGUNDO NOMBRE","PRIMER APELLIDO","SEGUNDO APELLIDO","FECHA DE LA ATENCIÓN","ESTADO ACCESO A OFERTA")

Consulta = Consulta[c("Group.1",Variables)]
setnames(Consulta, old = "Group.1", new = "Archivo")

Archivos_Original = Original %>% group_by(Archivo) %>% summarise(Total=n())

Archivos_Original =Reduce(function(x,y) merge(x = x, y = y, by = c("Archivo"), all.x=TRUE), list(Consulta,
                                                                                                 Archivos_Original))#Unión de datos de hogares


# Conteos por archivo dataframe Original
library("dplyr", lib.loc="~/R/R-3.6.3/library")
library("tidyr", lib.loc="~/R/R-3.6.3/library")

# Conteos por archivo de los que cruzaron por documento en dataframe Consulta
Archivos_Consulta_1 = Consulta_1 %>% group_by(Archivo) %>% summarise(Consulta_1=n())
Archivos_Consulta_2 = Consulta_2 %>% group_by(Archivo) %>% summarise(Consulta_2=n())
Archivos_Consulta_3 = Consulta_3 %>% group_by(Archivo) %>% summarise(Consulta_3=n())
Archivos_Consulta_4 = Consulta_4 %>% group_by(Archivo) %>% summarise(Consulta_4=n())
Archivos_Consulta_5 = Consulta_5 %>% group_by(Archivo) %>% summarise(Consulta_5=n())
Archivos_Consulta_6 = Consulta_6 %>% group_by(Archivo) %>% summarise(Consulta_6=n())


Archivos_Consulta =Reduce(function(x,y) merge(x = x, y = y, by = c("Archivo"), all.x=TRUE), list(Archivos_Consulta_1,
                                                                                                 Archivos_Consulta_2,
                                                                                                 Archivos_Consulta_3,
                                                                                                 Archivos_Consulta_4,
                                                                                                 Archivos_Consulta_5,
                                                                                                 Archivos_Consulta_6))#Unión de datos de hogares


rm(list = ls()[ls() %in% grep("^Archivos_Consulta_",ls(),value = TRUE)])
