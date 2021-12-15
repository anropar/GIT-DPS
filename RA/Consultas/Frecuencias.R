library(tidyr)
library(dplyr)
library(data.table)
library(reshape2)

#

MUNICIPIOS =read_excel("MUNICIPIOS.xlsx", sheet = "Municipios", skip = 10)
MUNICIPIOS =MUNICIPIOS[1:1122,c("Código...3","Nombre...2","Nombre...4")]

setnames(MUNICIPIOS, old = c("Código...3","Nombre...2","Nombre...4"),
         new = c("CodigoMunicipio","Departamento","Municipio"))#Cambio de nombre de columnas

INTEGRANTES=DATA[c("CodigoMunicipio")] %>% group_by(CodigoMunicipio) %>% summarise(TOTALPERSONAS=n())
HOGARES=DATA[!duplicated(paste(DATA$MARCA,DATA$A01)),c("A01","CodigoMunicipio")] %>% group_by(CodigoMunicipio) %>% summarise(TOTALHOGARES=n())

ESTADO= reshape2::dcast(data=DATA[!duplicated(paste(DATA$MARCA,DATA$A01)), c("CodigoMunicipio","EstadoHogar")],
                        CodigoMunicipio ~ EstadoHogar,
                        fun.aggregate = length,
                        value.var = "EstadoHogar")#Genera frecuencias en columnas de la variable definida

setnames(ESTADO, old = "NA", new = "Sindato_Estado")

ZONA= reshape2::dcast(data=DATA[!duplicated(paste(DATA$MARCA,DATA$A01)),],
                      CodigoMunicipio ~ A04,
                      fun.aggregate = length,
                      value.var = "A04")#Genera frecuencias en columnas de la variable definida

IPM= reshape2::dcast(data=DATA[!duplicated(paste(DATA$MARCA,DATA$A01)),c("CodigoMunicipio","denominacionIPM")],
                        CodigoMunicipio ~ denominacionIPM,
                        fun.aggregate = length,
                        value.var = "denominacionIPM")#Genera frecuencias en columnas de la variable definida

setnames(IPM, old = c("Var.2","NO POBRE","POBRE"), new = c("Sindato_IPM","NO_POBRE_IPM","POBRE_IPM"))

LP= reshape2::dcast(data=DATA[!duplicated(paste(DATA$MARCA,DATA$A01)),c("CodigoMunicipio","denominacionLP")],
                     CodigoMunicipio ~ denominacionLP,
                     fun.aggregate = length,
                     value.var = "denominacionLP")#Genera frecuencias en columnas de la variable definida

setnames(LP, old = c("Var.2","NO POBRE","POBRE","POBRE EXTREMO"), new = c("Sindato_LP","NO_POBRE_LP","POBRE_LP","POBRE_EXTREMO_IPM"))

#LOGROS=DATA[!duplicated(paste(DATA$MARCA,DATA$A01)),c("CodigoMunicipio", grep("^logro",names(DATA),value = TRUE)[1:26])] %>%
#  gather(category, val, -c(CodigoMunicipio)) %>%
#  na.omit() %>%
#  group_by(CodigoMunicipio, category, val) %>%
#  summarise(new = n()) %>%
#  spread(val, new, fill = 0)

#ALCANZADO    =LOGROS[c("CodigoMunicipio","category","ALCANZADO")] %>% spread(category, ALCANZADO)
#POR_ALCANZAR =LOGROS[c("CodigoMunicipio","category","POR ALCANZAR")] %>% spread(category, `POR ALCANZAR`)

#setnames(ALCANZADO, old = names(ALCANZADO)[-1],
#         new = paste(toupper(names(ALCANZADO)[-1]),"F","A",sep = "_"))

#setnames(POR_ALCANZAR, old = names(POR_ALCANZAR)[-1],
#         new = paste(toupper(names(POR_ALCANZAR)[-1]),"F","PA",sep = "_"))

DATA_Municipal_HOG=Reduce(function(x,y) merge(x = x, y = y, by = c("CodigoMunicipio"), all.x=TRUE), list(ZONA,ESTADO,IPM,LP))#Se unen los dataframe de frecuencias de individuos.


DISCAPACIDAD= reshape2::dcast(data=DATA,
                              CodigoMunicipio ~ Discapacidad,
                              fun.aggregate = length,
                              value.var = "Discapacidad")#Genera frecuencias en columnas de la variable definida

setnames(DISCAPACIDAD, old = c("NO","SI"),
         new = c("DISCAPACIDADNO","DISCAPACIDADSI"))

SEXO= reshape2::dcast(data=DATA,
                      CodigoMunicipio ~ E03,
                      fun.aggregate = length,
                      value.var = "E03")#Genera frecuencias en columnas de la variable definida

setnames(SEXO, old = c("HOMBRE","MUJER"),
         new = c("SEXOHOMBRE","SEXOMUJER"))

# SEXO$SEXOINTERSEXUAL=0#No hay casos de intersexuales. Se agrega para conservar la estructura.

GRUPOSETAREO = reshape2::dcast(data=DATA,
                               CodigoMunicipio ~ CICLOVITAL,
                               fun.aggregate = length,
                               value.var = "CICLOVITAL")#Genera frecuencias en columnas de la variable definida

GRUPOSETINICOS= reshape2::dcast(data=DATA,
                                CodigoMunicipio ~ E08,
                                fun.aggregate = length,
                                value.var = "E08")#Genera frecuencias en columnas de la variable definida

VICTIMA= reshape2::dcast(data=DATA,
                                CodigoMunicipio ~ Victimas,
                                fun.aggregate = length,
                                value.var = "Victimas")#Genera frecuencias en columnas de la variable definida

setnames(VICTIMA, old = c("1","0"), new = c("SIVICTIMA","NOVICTIMA"))

setnames(GRUPOSETINICOS, old = c("Indígena","Rom o Gitano","Raizal del archipiélago","Afrodescendiente (negro, mulato, cimarron u otro)","Palenquero","Ninguno de los anteriores","NA"),
         new = c("INDIGENA","ROM","RAIZAL","AFRODESCENDIENTE","PALENQUERO","SIN ETNIA","SINDATO_ETNIA"))

#LOGROS=DATA[c("CodigoMunicipio",grep("_I",names(DATA),value = TRUE)[-(1)])] %>%
#  gather(category, val, -c(CodigoMunicipio)) %>%
#  na.omit() %>%
#  group_by(CodigoMunicipio, category, val) %>%
#  summarise(new = n()) %>%
#  spread(val, new, fill = 0)

#ALCANZADO    =LOGROS[c("CodigoMunicipio","category","ALCANZADO")] %>% spread(category, ALCANZADO)
#POR_ALCANZAR =LOGROS[c("CodigoMunicipio","category","POR ALCANZAR")] %>% spread(category, `POR ALCANZAR`)

#setnames(ALCANZADO, old = names(ALCANZADO)[-1],
#         new = paste(toupper(names(ALCANZADO)[-1]),"A",sep = "_"))

#setnames(POR_ALCANZAR, old = names(POR_ALCANZAR)[-1],
#         new = paste(toupper(names(POR_ALCANZAR)[-1]),"PA",sep = "_"))

DATA_Municipal_INT=Reduce(function(x,y) merge(x = x, y = y, by = c("CodigoMunicipio"), all.x=TRUE), list(INTEGRANTES,HOGARES,SEXO,GRUPOSETAREO,DISCAPACIDAD,GRUPOSETINICOS,VICTIMA))#Se unen los dataframe de frecuencias de individuos.


DATA_Municipal=merge(DATA_Municipal_HOG,DATA_Municipal_INT,by="CodigoMunicipio",all.x=TRUE)
DATA_Municipal=merge(MUNICIPIOS,DATA_Municipal, by="CodigoMunicipio", all.y=TRUE)#Se genera el archivo de frecuencias municipales.

variables = c("Departamento","CodigoMunicipio","Municipio",
              grep("DISCAPACIDAD",names(DATA_Municipal), value = TRUE),
              grep("SEXO",names(DATA_Municipal), value = TRUE),
              "1-PrimeraInfancia","2-Ninez","3-Adolescencia","4-Juventud","5-Adulto","6-AdultoMayor",
              "AFRODESCENDIENTE","INDIGENA","PALENQUERO","RAIZAL","ROM","SIN ETNIA","SINDATO_ETNIA",
              grep("TOTAL",names(DATA_Municipal), value = TRUE),
              "CABECERA MUNICIPAL","CENTRO POBLADO","RURAL DISPERSO",setdiff(names(DATA_Municipal),variables))

variables_1 = c("Departamento","CodigoMunicipio","Municipio",
              grep("DISCAPACIDAD",names(DATA_Municipal), value = TRUE),
              grep("SEXO",names(DATA_Municipal), value = TRUE),
              "1-PrimeraInfancia","2-Ninez","3-Adolescencia","4-Juventud","5-Adulto","6-AdultoMayor",
              "AFRODESCENDIENTE","INDIGENA","PALENQUERO","RAIZAL","ROM","SIN ETNIA","SINDATO_ETNIA",
              grep("TOTAL",names(DATA_Municipal), value = TRUE),
              "CABECERA MUNICIPAL","CENTRO POBLADO","RURAL DISPERSO",setdiff(names(DATA_Municipal),variables))

DATA_Municipal=DATA_Municipal[variables_1]#Se ordenan las variables siguiendo el orden previo de los archivos de frecuencias.

view(dfSummary(as.data.frame(DATA_Municipal)))
