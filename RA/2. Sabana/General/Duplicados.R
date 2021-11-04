
# Duplicados Sisben IV
Error_duplicidad_IDHogar=DATA_HOG[duplicated(DATA_HOG$A01) | duplicated(DATA_HOG$A01, fromLast = T),c("CODIGO DEPARTAMENTO DANE","CODIGO MUNICIPIO DANE","A01")]#Detecta IDHogar duplicados a nivel de hogar.
Error_duplicidad_IDIntegrante=DATA[duplicated(DATA$IdIntegrante) | duplicated(DATA$IdIntegrante,fromLast = T) & !is.na(DATA$IdIntegrante),c("CODIGO DEPARTAMENTO DANE","CODIGO MUNICIPIO DANE","A01","IdIntegrante")]#Detecta IdIntegrante a nivel de hogar.

# Error_duplicidad_Identificacion = DATA[duplicated(paste(DATA$E08,DATA$E09)) | duplicated(paste(DATA$E08,DATA$E09),fromLast = T) & !is.na(paste(DATA$E08,DATA$E09)),c("CODIGO DEPARTAMENTO DANE","CODIGO MUNICIPIO DANE","A01","IdIntegrante", "E08", "FECHA DE NACIMIENTO")]#Detecta IdIntegrante a nivel de hogar.

source("Fonetico.R")#Se utiliza para dirigir las salidas a las carpetas definidas.

Error_duplicidad_Integrante_Fonetico = DATA[c("CODIGO DEPARTAMENTO DANE","CODIGO MUNICIPIO DANE","A01","IdIntegrante","PRIMER NOMBRE","SEGUNDO NOMBRE","PRIMER APELLIDO","SEGUNDO APELLIDO","TIPO DOCUMENTO","NUMERO DOCUMENTO","FECHA DE NACIMIENTO","Total_Personas")]
Error_duplicidad_Integrante_Fonetico$Fonetico = paste(fonetico(Error_duplicidad_Integrante_Fonetico$`PRIMER NOMBRE`),
                                                      fonetico(Error_duplicidad_Integrante_Fonetico$`SEGUNDO NOMBRE`),
                                                      fonetico(Error_duplicidad_Integrante_Fonetico$`PRIMER APELLIDO`),
                                                      fonetico(Error_duplicidad_Integrante_Fonetico$`SEGUNDO APELLIDO`),
                                                      Error_duplicidad_Integrante_Fonetico$`FECHA DE NACIMIENTO`)

Error_duplicidad_Integrante_Fonetico = Error_duplicidad_Integrante_Fonetico[duplicated(Error_duplicidad_Integrante_Fonetico$Fonetico)|
                                                                            duplicated(Error_duplicidad_Integrante_Fonetico$Fonetico,fromLast=TRUE),c("CODIGO DEPARTAMENTO DANE","CODIGO MUNICIPIO DANE","A01","IdIntegrante",grep("E01",names(DATA),value = T),"FECHA DE NACIMIENTO","Fonetico","TIPO DOCUMENTO","NUMERO DOCUMENTO","Total_Personas")]

Error_duplicidad_Integrante_Fonetico = as.data.frame(Error_duplicidad_Integrante_Fonetico)

Error_duplicidad_Integrante_Fonetico = Error_duplicidad_Integrante_Fonetico %>% mutate(Item=dense_rank(Fonetico))
