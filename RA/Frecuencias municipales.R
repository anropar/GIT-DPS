###########################
# Informe mensual Febrero #
#        DASHBOARD        #
#   Andres Romero Parra   #
#                         #
###########################
setwd(Entradas)

BaseGestion_2021 = read_delim("BaseGestion Hogares Acompañados 2021.txt", ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
 
# LOGROS_HOG = read_delim("Unidos_Logros_Hogar_20211203.txt", 
#                          "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
#                          trim_ws = TRUE)
# 
# LOGROS_INT = read_delim("Unidos_Logros_Integrante_20211203.txt", 
#                          "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
#                          trim_ws = TRUE)

DATA = DATA[DATA$IdIntegrante %in% BaseGestion_2021$idIntegranteHogar,]
DATA = merge(DATA, BaseGestion_2021[c("idIntegranteHogar","denominacionIPM","EstadoHogar")], by.x = "IdIntegrante", by.y = "idIntegranteHogar")

##########
#HOGARES
##########
PERFILES = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","PERFIL_HOG")],
                            A03_1 ~ PERFIL_HOG,
                            fun.aggregate = length,
                            value.var = "PERFIL_HOG")#Genera frecuencias en columnas de la variable definida

setnames(PERFILES, old = c("AFIANZAMIENTO") , new = c("PRODUCTIVO"))

D01 = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","D01")],
                       A03_1 ~ D01,
                       fun.aggregate = length,
                       value.var = "D01")#Genera frecuencias en columnas de la variable definida

setnames(D01, old = c("1","2","3","4","5","NA") , new = c("En arriendo o subarriendo","Propia, la están pagando","Propia, totalmente pagada","Con permiso del propietario","Posesión sin título, ocupante de hecho","Sin información_D01"))


B01 = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","B01")],
                      A03_1 ~ B01,
                      fun.aggregate = length,
                      value.var = "B01")#Genera frecuencias en columnas de la variable definida

setnames(B01, old = c("1","2","3","4","5") , new = c("Casa","Apartamento","Cuarto (s)","Otro tipo de vivienda","Vivienda indígena"))

ZONA = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","A04")],
                       A03_1 ~ A04,
                       fun.aggregate = length,
                       value.var = "A04")#Genera frecuencias en columnas de la variable definida

setnames(ZONA, old = c("1","2","3") , new = c("Cabecera Municipal","Centro Poblado","Rural Disperso"))

ESTADO = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","EstadoHogar")],
                        A03_1 ~ EstadoHogar,
                       fun.aggregate = length,
                       value.var = "EstadoHogar")#Genera frecuencias en columnas de la variable definida

# 
# LP = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","denominacionLP")],
#                        A03_1 ~ denominacionLP,
#                        fun.aggregate = length,
#                        value.var = "denominacionLP")#Genera frecuencias en columnas de la variable definida
# 
# setnames(LP, old = c("1","2","3") , new = c("NO_POBRE_LP","POBRE_LP","POBRE_EXTREMO_LP"))

IPM = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","denominacionIPM")],
                      A03_1 ~ denominacionIPM,
                      fun.aggregate = length,
                      value.var = "denominacionIPM")#Genera frecuencias en columnas de la variable definida

setnames(IPM, old = c("NO POBRE","POBRE","NO DETERMINADO") , new = c("NO_POBRE_IPM","POBRE_IPM","NO_DETERMINADO_IPM"))

VICTIMA= reshape2::dcast(data=DATA[DATA$VICTIMA==1 & !duplicated(DATA$A01),],
                         A03_1 ~ VICTIMA,
                         fun.aggregate = length,
                         value.var = "VICTIMA")#Reshape2

colnames(VICTIMA)[2]="VICTIMA"

#############
#INTEGRANTES#
#############
DATA$Discapacidad = +(apply(DATA[grep("F01", names(DATA), value = T)[-8]] == 1, 1, any))

DISCAPACIDAD = DATA %>% group_by(A03_1) %>% summarise(Discapacidad=sum(Discapacidad,na.rm = T))

INTEGRANTES = DATA %>% group_by(A03_1) %>% summarise(Totalhogares=n_distinct(A01),
                                                     Totalintegrantes=n())

SEXO= reshape2::dcast(data=DATA[c("A03_1","E03")],
                      A03_1 ~ E03,
                      fun.aggregate = length,
                      value.var = "E03")#Genera frecuencias en columnas de la variable definida

setnames(SEXO, old = c("1","2") , new = c("Hombre","Mujer"))

PIRAMIDES = reshape2::dcast(data=DATA[c("A03_1","CICLOVITAL")],
                           A03_1 ~ CICLOVITAL,
                           fun.aggregate = length,
                           value.var = "CICLOVITAL")

# setnames(PIRAMIDES, old = c("NA") , new = c("Sin información_PIRAMIDES"))

MUJERJEFE= reshape2::dcast(data=DATA[DATA$Mujer_jefe==1,],
                           A03_1 ~ Mujer_jefe,
                           fun.aggregate = length,
                           value.var = "Mujer_jefe")#Reshape2

MUJERJEFE=MUJERJEFE[c(-3)]
colnames(MUJERJEFE)[2]="Mujerjefe"

##################
# Unión de datos #
##################

DATA_Municipal_HOG = Reduce(function(x,y) merge(x = x, y = y, by = c("A03_1"), all.x=TRUE), list(B01,D01,ZONA,IPM,DISCAPACIDAD,INTEGRANTES,PERFILES,VICTIMA,MUJERJEFE,SEXO,PIRAMIDES,ESTADO))#Se unen los dataframe de frecuencias de individuos.
setnames(DATA_Municipal_HOG, old = "A03_1", new = "CodigoMunicipio")

MUNICIPIOS = read_excel("~/Datos/2018/MUNICIPIOS.xlsx")
setnames(MUNICIPIOS, old = c("CODIGO_MUNICIPIO","DEPARTAMENTO","MUNICIPIO"), new = c("CodigoMunicipio","Departamento","Municipio"))

DATA_Municipal_HOG = merge(DATA_Municipal_HOG, MUNICIPIOS, by="CodigoMunicipio")

DATA_Municipal_HOG$Periodo = 2021

#Estado de logros por hogar agregado municipal
BaseGestion_2021$Periodo = 2021

df_Municipal = BaseGestion_2021[!duplicated(BaseGestion_2021$idHogar),c("Periodo","Departamento","Municipio","CodigoMunicipio",grep("_I_",grep("logro",names(BaseGestion_2021),value = TRUE),value = TRUE, invert = T))]  %>% gather(key = "LOGRO",value = "ESTADO",grep("_I_",grep("logro",names(BaseGestion_2021),value = TRUE),value = TRUE, invert = T)) %>% group_by(Periodo,Departamento,Municipio,CodigoMunicipio, LOGRO, ESTADO) %>% count()
df_Municipal = df_Municipal %>% mutate(LOGRO = strsplit(as.character(LOGRO), "_")[[1]][1])
df_Municipal = df_Municipal[df_Municipal$LOGRO %in% c("logro09","logro10","logro11","logro15","logro21","logro22","logro23","logro24","logro26"),]
colnames(df_Municipal)[7]="CANTIDAD"

#Estado de logros por integrante agregado municipal
df_1_Municipal = BaseGestion_2021[c("Periodo","Departamento","Municipio","CodigoMunicipio",grep("_I",names(BaseGestion_2021),value = TRUE))]  %>% gather(key = "LOGRO",value = "ESTADO",grep("_I",names(BaseGestion_2021),value = TRUE)) %>% group_by(Periodo,Departamento,Municipio,CodigoMunicipio, LOGRO, ESTADO) %>% count()
df_1_Municipal = df_1_Municipal %>% mutate(LOGRO = strsplit(as.character(LOGRO), "_")[[1]][1])
df_1_Municipal$LOGRO = paste0(df_1_Municipal$LOGRO,"_I")
df_1_Municipal = df_1_Municipal[!(df_1_Municipal$LOGRO %in% paste(c("logro09","logro10","logro11","logro15","logro21","logro22","logro23","logro24","logro26"),"_I",sep = "")),]
colnames(df_1_Municipal)[7]="CANTIDAD"

########################################
# Unión de datos con otras operaciones #
########################################
UNIDOS_HOGARES_MUNICIPAL_06022020 <- read_delim("~/Datos/2020/Tablero de control/UNIDOS_HOGARES_MUNICIPAL_06022020.csv",
                                                ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
                                                trim_ws = TRUE)

MUNICIPAL_ESTADO_DE_LOGROS_07022020 <- read_delim("~/Datos/2020/Tablero de control/MUNICIPAL_ESTADO_DE_LOGROS_07022020.csv", 
                                                    ";", escape_double = FALSE, trim_ws = TRUE)

MUNICIPAL_ESTADO_DE_LOGROS_I_07022020 <- read_delim("~/Datos/2020/Tablero de control/MUNICIPAL_ESTADO_DE_LOGROS_I_07022020.csv", 
                                                    ";", escape_double = FALSE, trim_ws = TRUE)

UNIDOS_HOGARES_MUNICIPAL = rbind.fill(DATA_Municipal_HOG, UNIDOS_HOGARES_MUNICIPAL_06022020)
MUNICIPAL_ESTADO_DE_LOGROS = rbind.fill(df_Municipal, MUNICIPAL_ESTADO_DE_LOGROS_07022020)
MUNICIPAL_ESTADO_DE_LOGROS_I = rbind.fill(df_1_Municipal, MUNICIPAL_ESTADO_DE_LOGROS_I_07022020)

UNIDOS_HOGARES_MUNICIPAL = UNIDOS_HOGARES_MUNICIPAL[c(names(UNIDOS_HOGARES_MUNICIPAL_06022020),"NO_DETERMINADO_IPM")]
UNIDOS_HOGARES_MUNICIPAL$VICTIMA[is.na(UNIDOS_HOGARES_MUNICIPAL$VICTIMA)]=0
UNIDOS_HOGARES_MUNICIPAL[is.na(UNIDOS_HOGARES_MUNICIPAL)] = 0

MUNICIPIOS <- read_excel("~/Datos/2020/REQUERIMIENTOS/Generacion de cortes/Entradas/MUNICIPIOS.xlsx", 
                         sheet = "Municipios", skip = 10)

setnames(MUNICIPIOS, old = c("Nombre...2","Nombre...4","Código...3"), new = c("Departamento","Municipio", "CodigoMunicipio"))

UNIDOS_HOGARES_MUNICIPAL$CodigoMunicipio = str_pad(UNIDOS_HOGARES_MUNICIPAL$CodigoMunicipio, width=5, pad="0")
MUNICIPAL_ESTADO_DE_LOGROS$CodigoMunicipio = str_pad(MUNICIPAL_ESTADO_DE_LOGROS$CodigoMunicipio, width=5, pad="0")
MUNICIPAL_ESTADO_DE_LOGROS_I$CodigoMunicipio = str_pad(MUNICIPAL_ESTADO_DE_LOGROS_I$CodigoMunicipio, width=5, pad="0")


UNIDOS_HOGARES_MUNICIPAL = merge(UNIDOS_HOGARES_MUNICIPAL %>% select(-c("Departamento","Municipio")), 
                                 MUNICIPIOS %>% select(c("Departamento","Municipio","CodigoMunicipio")), by = "CodigoMunicipio", all.x = T)

MUNICIPAL_ESTADO_DE_LOGROS = merge(MUNICIPAL_ESTADO_DE_LOGROS %>% select(-c("Departamento","Municipio")), 
                                 MUNICIPIOS %>% select(c("Departamento","Municipio","CodigoMunicipio")), by = "CodigoMunicipio", all.x = T)

MUNICIPAL_ESTADO_DE_LOGROS_I = merge(MUNICIPAL_ESTADO_DE_LOGROS_I %>% select(-c("Departamento","Municipio")), 
                                 MUNICIPIOS %>% select(c("Departamento","Municipio","CodigoMunicipio")), by = "CodigoMunicipio", all.x = T)

UNIDOS_HOGARES_MUNICIPAL$Periodo = as.character(UNIDOS_HOGARES_MUNICIPAL$Periodo)
MUNICIPAL_ESTADO_DE_LOGROS$Periodo = as.character(MUNICIPAL_ESTADO_DE_LOGROS$Periodo)
MUNICIPAL_ESTADO_DE_LOGROS_I$Periodo = as.character(MUNICIPAL_ESTADO_DE_LOGROS_I$Periodo)

setwd("~/GitHub/GIT-DPS/RA/2. Sabana/Salidas/Dashboard")
write.csv2(UNIDOS_HOGARES_MUNICIPAL, file = paste("UNIDOS_MUNICIPAL",".csv", sep=""), row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(MUNICIPAL_ESTADO_DE_LOGROS, file = paste("MUNICIPAL_ESTADO_DE_LOGROS",".csv", sep=""), row.names = FALSE)
write.csv2(MUNICIPAL_ESTADO_DE_LOGROS_I, file = paste("MUNICIPAL_ESTADO_DE_LOGROS_I",".csv", sep=""), row.names = FALSE)
