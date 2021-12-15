
library(rgdal)
library(rgeos)
library(sp)
library(ggmap)
library(readr)
library(readxl)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(phonics)
library(foreign)
library(reader)
library("reshape2", lib.loc="~/R/win-library/3.5")
library("data.table", lib.loc="~/R/win-library/3.5")

#VICTIMAS    =read.table("~/Datos/2019/VICTIMAS/VICTIMAS_201908.txt","»", header=TRUE, encoding="ISO8859-1", stringsAsFactors=FALSE)

#UNIDOS 2018#
#############

HOGARES     =read.csv("~/Datos/2019/INICIALES/Base Gestion/BaseGestion_2016_2018_HOGARES_CORTE201903_20190402.csv",encoding = "UTF-8", sep=";")
INTEGRANTES =read.csv("~/Datos/2019/INICIALES/Base Gestion/BaseGestion_2016_2018_PERSONAS_CORTE201903_20190402.csv",encoding = "UTF-8", sep=";")
FORMULARIO_2018 =read.csv("~/Datos/2019/EJERCICIOS/EncuestaCaracterizacion_CorteFEBRERO_20190404/EncuestaCaracterizacion_CorteFEBRERO_20190409.csv", encoding="ISO-8859-1", sep="|")

colnames(HOGARES)[1]          = c("IDHOGAR")
colnames(INTEGRANTES)[c(1,4)] = c("IDHOGAR","ID_INTEGRANTE")
colnames(FORMULARIO_2018)[c(1,2)]    = c("IDHOGAR","ID_INTEGRANTE")

oldnames = grep("LOGRO", names(INTEGRANTES), value=TRUE)
newnames = grep("LOGRO", names(HOGARES), value=TRUE)
for(i in 1:26) names(INTEGRANTES)[names(INTEGRANTES) == oldnames[i]] = newnames[i]
rm(list = ls(pattern = "names"))

INTEGRANTES=merge(INTEGRANTES,HOGARES[c(1:2,4:5,7)], by = c("IDHOGAR","MARCA"), all.x = TRUE)
HOGARES =HOGARES[!duplicated(HOGARES$IDHOGAR),]

HOGARES=HOGARES[HOGARES$MARCA=="2018",]
INTEGRANTES=INTEGRANTES[INTEGRANTES$IDHOGAR %in% HOGARES$IDHOGAR,]

variables_FORMULARIO_2018=c("IDHOGAR","ID_INTEGRANTE","idMunicipio","E08_GrupoEtnico","E11_Parentesco",
                     "D01_LaViviendaLote","B1_TipoVivienda")

variables_hogares=c("IDHOGAR","DEPARTAMENTO","MUNICIPIO","COD_MUNICIPIO","ZONA","ESTADOHOGAR","DENOMINACION_LP","DENOMINACIONIPM",grep("^LOGRO",names(HOGARES),value = TRUE))

variables_integrante=c("IDHOGAR","ID_INTEGRANTE","EDAD","SEXO","DISCAPACIDAD","FECHANACIMIENTO",grep("DOCUMENTO|PRIMER|SEGUNDO|^LOGRO",names(INTEGRANTES), value = TRUE))

FORMULARIO_2018=FORMULARIO_2018[variables_FORMULARIO_2018]
HOGARES= HOGARES[variables_hogares]
INTEGRANTES=INTEGRANTES[variables_integrante]

setnames(FORMULARIO_2018, old = variables_FORMULARIO_2018,
         new = c("A01","idIntegranteHogar","CodigoMunicipio","E08","E11","D01","B01"))#Renombra variables

setnames(HOGARES, old = variables_hogares,
         new = c("A01","Departamento","Municipio","CodigoMunicipio","A04","EstadoHogar","denominacionLP","denominacionIPM",tolower(grep("^LOGRO",names(HOGARES),value = TRUE))))#Renombra variables

setnames(INTEGRANTES, old = variables_integrante,
         new = c("A01","idIntegranteHogar","EdadCaracterizacion","E03","Discapacidad","E02","E01_a","E01_b","E01_c","E01_d","E05","E06",paste(tolower(grep("^LOGRO",names(INTEGRANTES),value = TRUE)),"_I", sep = "")))#Renombra variables

UNIDOS_2018=merge(HOGARES,INTEGRANTES,by="A01")
UNIDOS_2018=merge(UNIDOS_2018,FORMULARIO_2018[c(-1,-3)],by="idIntegranteHogar", all.x = TRUE)

UNIDOS_2018$logro27= NA
UNIDOS_2018$logro28= NA
UNIDOS_2018$logro27_I= NA
UNIDOS_2018$logro28_I= NA

UNIDOS_2018$Periodo="2018"
UNIDOS_2018=UNIDOS_2018[c(1:7,36:45,72:75,10:35,76,77,46:71,78,79,8,9,80)]

UNIDOS_2018$A04=recode(UNIDOS_2018$A04, `CABECERA MUNICIPAL` = "1",
                                        `CENTRO POBLADO` = "2",
                                        `RURAL DISPERSO` = "3")

UNIDOS_2018$B01=recode(UNIDOS_2018$B01, `a) Casa` = "1",
                                        `b) Apartamento` = "2",
                                        `c) Cuarto(s)` = "3",
                                        `e) Otro tipo de vivienda (carpa, tienda, vagón, embarcación, cueva, refugio natural, puente, etc.)`= "4",
                                        `d) Vivienda tradicional étnica` = "5")


UNIDOS_2018$E03=recode(UNIDOS_2018$E03, `HOMBRE` = "1",
                                        `MUJER` = "2")

UNIDOS_2018$Discapacidad=recode(UNIDOS_2018$Discapacidad, `NO` = 0, `SI` = 1)

UNIDOS_2018$E02=as.Date(UNIDOS_2018$E02, "%Y/%m/%d")

UNIDOS_2018$E05=recode(UNIDOS_2018$E05,  `RC` = "1",
                                         `TI` = "2",
                                         `CC` = "3",
                                         `CE` = "4",
                                         `DNI` = "5",
                                         `Pasaporte` = "6",
                                         `ND` = "0")

UNIDOS_2018$E08=recode(UNIDOS_2018$E08,  `Indígena` = "1",
                                         `Rom o Gitano` = "2",
                                         `Raizal del archipiélago` = "3",
                                         `Afrodescendiente (negro, mulato, cimarron u otro)` = "4",
                                         `Palenquero` = "5",
                                         `Ninguno de los anteriores` = "6")

UNIDOS_2018$D01=recode(UNIDOS_2018$D01,  `En arriendo o subarriendo` = "1",
                                         `Propia, la están pagando` = "2",
                                         `Propia, totalmente pagada` = "3",
                                         `En usufructo` = "4",
                                         `Posesión sin título (Ocupante de hecho)` = "5",
                                         `Título de propiedad colectiva` = "6")


UNIDOS_2018$E11=recode(UNIDOS_2018$E11,  `Jefe` = "1",
                                         `Cónyuge o Compañera (o)` = "2",
                                         `Hijos / Hijastros` = "3",
                                         `Nietos` = "4",
                                         `Padres` = "5",
                                         `Hermanos` = "6",
                                         `Yerno o nuera` = "7",
                                         `Abuelos` = "8",
                                         `Suegros` = "9",
                                         `Tíos` = "10",
                                         `Sobrinos` = "11",
                                         `Primos` = "12",
                                         `Cuñados` = "13",
                                         `Otros parientes` = "14")


UNIDOS_2018[c("A04","E03","E05","E08","D01","B01","Discapacidad","E11")]=lapply(UNIDOS_2018[c("A04","E03","E05","E08","D01","B01","Discapacidad","E11")],as.character)

#UNIDOS 2019#
#############
FORMULARIO_20191  <- read_delim("~/Datos/2020/DATOS/Sabana_Caracterizacion_2019.txt","|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),trim_ws = TRUE)
UNIDOS_20191      <- read.csv("~/Datos/2020/DATOS/BaseGestion_2019.csv", sep=";")
FORMULARIO_2019   <- read_delim("~/Datos/2020/DATOS/Caracterizacion_DNP_20200211.txt","|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),trim_ws = TRUE)
UNIDOS_2019       <- read_excel("~/Datos/2020/DATOS/Calculos 20200211.xlsx", skip = 1)

LogrosHogar <- read_excel("~/Datos/2020/DATOS/Calculos 20200211.xlsx", sheet = "LogrosHogar", skip = 1)
LogroIntegrante <- read_excel("~/Datos/2020/DATOS/Calculos 20200211.xlsx", sheet = "LogrosIntegrante", skip = 1)
IPM <- read_excel("~/Datos/2020/DATOS/Calculos 20200211.xlsx", sheet = "IPM", skip = 0)
LP <- read_excel("~/Datos/2020/DATOS/Calculos 20200211.xlsx", sheet = "LP", skip = 0)

setnames(LogroIntegrante, old = c("fechaCalculo",grep("logro",names(LogroIntegrante), value = TRUE)),
         new = paste(c("fechaCalculo",grep("logro",names(LogroIntegrante), value = TRUE)),"_I", sep = ""))

setnames(IPM, old = c("fechaCalculo"),
         new = c("fechaCalculo_IPM"))

setnames(LP, old = c("fechaCalculo"),
         new = c("fechaCalculo_LP"))

UNIDOS_2019=Reduce(function(x,y) merge(x = x, y = y, by = c("idHogar"), all.x=TRUE), list(LogrosHogar,LP,IPM))
UNIDOS_2019=merge(UNIDOS_2019,LogroIntegrante[c("idHogar",setdiff(names(LogroIntegrante),names(UNIDOS_2019)))],by="idHogar")
UNIDOS_2019=UNIDOS_2019[intersect(names(UNIDOS_2019),names(UNIDOS_20191))]
UNIDOS_2019=merge(UNIDOS_2019, UNIDOS_20191[c("idIntegranteHogar",setdiff(names(UNIDOS_20191),names(UNIDOS_2019)))], by="idIntegranteHogar")
UNIDOS_2019=UNIDOS_2019[names(UNIDOS_20191)]

FORMULARIO_2019$E03=ifelse(FORMULARIO_2019$idIntegranteHogar==1717308,3,FORMULARIO_2019$E03)#Intersexual

FORMULARIO_2019$Discapacidad=rowSums(FORMULARIO_2019[grep("E15", names(FORMULARIO_2019), value = TRUE)][-8]==1)
FORMULARIO_2019$Discapacidad=ifelse(FORMULARIO_2019$Discapacidad>0,1,0)

UNIDOS_2019=UNIDOS_2019[intersect(names(UNIDOS_2019),names(UNIDOS_2018))]
FORMULARIO_2019=FORMULARIO_2019[intersect(names(FORMULARIO_2019),names(UNIDOS_2018))]

UNIDOS_2019$logro12= NA
UNIDOS_2019$logro19= NA
UNIDOS_2019$logro12_I= NA
UNIDOS_2019$logro19_I= NA

UNIDOS_2019$Periodo="2019"


UNIDOS_2019=merge(UNIDOS_2019[c(intersect(names(FORMULARIO_2019),names(UNIDOS_2019)),
                    setdiff(names(UNIDOS_2019),names(FORMULARIO_2019)))],
                  FORMULARIO_2019[c("idIntegranteHogar",setdiff(names(FORMULARIO_2019),names(UNIDOS_2019)))],
                  by="idIntegranteHogar")

UNIDOS_2019=UNIDOS_2019[names(UNIDOS_2018)]

UNIDOS_2019$E11=recode(UNIDOS_2019$E11,  `Jefe del hogar` = "1",
                                         `Cónyuge o Compañero(a)` = "2",
                                         `Hijo(a), hijastro(a), hijo(a) adoptivo(a)` = "3",
                                         `Nieto(a)` = "4",
                                         `Padre, madre, padrastro, madrastra` = "5",
                                         `Hermano(a)` = "6",
                                         `Yerno / Nuera` = "7",
                                         `Abuelo(a)` = "8",
                                         `Suegro(a)` = "9",
                                         `Tío(a)` = "10",
                                         `Sobrino(a)` = "11",
                                         `Primo(a)` = "12",
                                         `Cuñado(a)` = "13",
                                         `Otro pariente` = "14",
                                         `Empleado(a) del servicio doméstico` = "15",
                                         `Pariente del servicio doméstico` = "16",
                                         `Pensionista` = "17",
                                         `Pariente de pensionista` = "18",
                                         `No pariente` = "19")


rm(list=ls()[!ls() %in% c("UNIDOS_2018","UNIDOS_2019")])

#UNIDOS2017-2019
#################
acentos=function(text){
  text=gsub("Á","A",text)
  text=gsub("É","E",text)
  text=gsub("Í","I",text)
  text=gsub("Ó","O",text)
  text=gsub("Ú","U",text)
  return(text)
}


UNIDOS_2018_2019=rbind(UNIDOS_2018, UNIDOS_2019)

UNIDOS_2018_2019[c("A04","E03","E05","E08","D01","B01","Discapacidad","E11")]=lapply(UNIDOS_2018_2019[c("A04","E03","E05","E08","D01","B01","Discapacidad","E11")],as.factor)

UNIDOS_2018_2019$MUJERJEFE=ifelse(UNIDOS_2018_2019$E11==1 & UNIDOS_2018_2019$E03==2,1,0)

UNIDOS_2018_2019$Departamento=recode(UNIDOS_2018_2019$Departamento,`NORTE DE SANTANDER` = "N. DE SANTANDER",
                                     `BOGOTÁ, D.C` = "BOGOTA, D.C.",
                                     `ARCHIPIÉLAGO DE SAN ANDRÉS Y PROVIDENCIA` = "SAN ANDRES")

UNIDOS_2018_2019$Municipio=recode(UNIDOS_2018_2019$Municipio,`NORTE DE SANTANDER` = "N. DE SANTANDER",
                                     `BOGOTÁ, D.C` = "BOGOTA, D.C.",
                                     `ARCHIPIÉLAGO DE SAN ANDRÉS Y PROVIDENCIA` = "SAN ANDRES",
                                     `MAGÜÍ` = "MAGUI",
                                     `TOGÜÍ`="TOGUI",
                                     `ITAGÜI`="ITAGUI")


UNIDOS_2018_2019$Departamento=acentos(UNIDOS_2018_2019$Departamento)
UNIDOS_2018_2019$Municipio   =acentos(UNIDOS_2018_2019$Municipio)

#######

UNIDOS_2018_2019= UNIDOS_2018_2019 %>% group_by(A01) %>% mutate(PERFIL_HOG = ifelse(any(EdadCaracterizacion<6) , "INICIAL",
                                                                          ifelse(any(EdadCaracterizacion>=6 & EdadCaracterizacion<=17),"CONSOLIDACION",
                                                                                 ifelse(all(EdadCaracterizacion>=18) & !all(EdadCaracterizacion>=65),"AFIANZAMIENTO",
                                                                                        ifelse(all(EdadCaracterizacion>=65),"MAYOR","NA")))))




UNIDOS_2018_2019= UNIDOS_2018_2019 %>% mutate(CICLOVITAL = ifelse(EdadCaracterizacion<=5, "Primera Infancia",
                                                        ifelse((EdadCaracterizacion>=6 & EdadCaracterizacion<=11),"Niñez",
                                                               ifelse((EdadCaracterizacion>=12 & EdadCaracterizacion<=17),"Adolescencia",
                                                                      ifelse((EdadCaracterizacion>=18 & EdadCaracterizacion<=24),"Juventud",
                                                                             ifelse((EdadCaracterizacion>=25 & EdadCaracterizacion<=59),"Adulto",
                                                                                    ifelse(EdadCaracterizacion>59,"Adulto mayor",
                                                                                           "NA")))))))




#VICTIMAS#
##########
Unidos_2018_Certificada <- read.csv2("~/Datos/2020/Tablero de control/10_FUENTES CERTIFICADAS/2018/Unidos_2018_Certificada.csv", sep="|")
Unidos_2019_Certificada <- read.csv2("~/Datos/2020/Tablero de control/10_FUENTES CERTIFICADAS/2019/Unidos_2019_Certificada.csv", sep="|")

Unidos_2018_Certificada$VICTIMA=ifelse(!is.na(Unidos_2018_Certificada$IDPERSONA_VICTIMAS),1,0)
Unidos_2019_Certificada$VICTIMA=ifelse(!is.na(Unidos_2019_Certificada$IDPERSONA_VICTIMAS),1,0)

VICTIMA_2018=Unidos_2018_Certificada[Unidos_2018_Certificada$VICTIMA==1,"IDINTEGRANTE"]
VICTIMA_2019=Unidos_2019_Certificada[Unidos_2019_Certificada$VICTIMA==1,"idIntegranteHogar"]

UNIDOS_2018_2019$VICTIMA_DESPL=ifelse((UNIDOS_2018_2019$Periodo==2018 & UNIDOS_2018_2019$idIntegranteHogar %in% VICTIMA_2018)|
                                       (UNIDOS_2018_2019$Periodo==2019 & UNIDOS_2018_2019$idIntegranteHogar %in% VICTIMA_2019),1,0)

#NIVEL DE AGREGACION: MUNICIPIOS#
#################################
ESTADOHOGAR= reshape2::dcast(data=UNIDOS_2018_2019,
                 CodigoMunicipio + Periodo ~ EstadoHogar,
                 fun.aggregate = length,
                 value.var = "EstadoHogar")#Reshape2


GRUPOSETINICOS= reshape2::dcast(data=UNIDOS_2018_2019,
                      CodigoMunicipio + Periodo ~ E08,
                      fun.aggregate = length,
                      value.var = "E08")#Reshape2

colnames(GRUPOSETINICOS)[3:9]=c("Indígena",
                                "Gitano(a) o ROM",
                                "Raizal",
                                "Afrodescendiente",
                                "Palenquero",
                                "Ninguno de los anteriores",
                                "Sin dato")

PIRAMIDES= reshape2::dcast(data=UNIDOS_2018_2019,
                 CodigoMunicipio + Periodo ~ CICLOVITAL,
                 fun.aggregate = length,
                 value.var = "CICLOVITAL")#Reshape2

SEXO= reshape2::dcast(data=UNIDOS_2018_2019,
            CodigoMunicipio + Periodo ~ E03,
            fun.aggregate = length,
            value.var = "E03")#Reshape2

colnames(SEXO)[3:5]=c("Hombre","Mujer","Intersexual")

PERFIL_HOG= reshape2::dcast(data=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),],
                  CodigoMunicipio + Periodo ~ PERFIL_HOG,
                  fun.aggregate = length,
                  value.var = "PERFIL_HOG")#Reshape2

TENENCIA= reshape2::dcast(data=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),],
                CodigoMunicipio + Periodo ~ D01,
                fun.aggregate = length,
                value.var = "D01")#Reshape2

colnames(TENENCIA)[3:9]=c("En arriendo o subarriendo",
                          "Propia, la están pagando",
                          "Propia, totalmente pagada",
                          "Con permiso del propietario",
                          "Posesión sin título, ocupante de hecho",
                          "Título de propiedad colectiva",
                          "Sin información_D01")

TIPO_VIVIENDA= reshape2::dcast(data=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),],
                     CodigoMunicipio + Periodo ~ B01,
                     fun.aggregate = length,
                     value.var = "B01")#Reshape2

colnames(TIPO_VIVIENDA)[3:9]=c("Casa",
                                "Apartamento",
                                "Cuarto (s)",
                                "Otro tipo de vivienda",
                                "Vivienda indígena",
                                "NULL",
                                "Sin información_B01")

ZONA= reshape2::dcast(data=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),],
            CodigoMunicipio + Periodo ~ A04,
            fun.aggregate = length,
            value.var = "A04")#Reshape2

colnames(ZONA)[3:5]=c("Cabecera Municipal",
                       "Centro Poblado",
                       "Rural Disperso")

IPM= reshape2::dcast(data=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),],
           CodigoMunicipio + Periodo ~ denominacionIPM,
           fun.aggregate = length,
           value.var = "denominacionIPM")#Reshape2

colnames(IPM)[3:4]=c("NO_POBRE_IPM","POBRE_IPM")

LP= reshape2::dcast(data=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),],
          CodigoMunicipio + Periodo ~ denominacionLP,
          fun.aggregate = length,
          value.var = "denominacionLP")#Reshape2

colnames(LP)[3:5]=c("NO_POBRE_LP","POBRE_LP","POBRE_EXTREMO_LP")

MUJERJEFE= reshape2::dcast(data=UNIDOS_2018_2019[UNIDOS_2018_2019$MUJERJEFE==1,],
                 CodigoMunicipio + Periodo ~ MUJERJEFE,
                 fun.aggregate = length,
                 value.var = "MUJERJEFE")#Reshape2

MUJERJEFE=MUJERJEFE[c(-4)]
colnames(MUJERJEFE)[3]="MUJERJEFE_sum"


#Se compila las frecuencias de las variables a nivel municipal

compilado=UNIDOS_2018_2019 %>% group_by(CodigoMunicipio, Periodo) %>% summarise(DISCAPACIDAD_sum=sum(as.numeric(as.character(Discapacidad)),na.rm = TRUE),
                                                                            TOTALINTEGRANTES_sum=n(),
                                                                                TOTALHOGARES_sum=n_distinct(A01))


VICTIMA_DESPL=UNIDOS_2018_2019[c("A01","CodigoMunicipio", "Periodo","VICTIMA_DESPL")] %>% group_by(A01) %>% mutate(VICTIMAS=ifelse(any(VICTIMA_DESPL==1),1,0))
VICTIMA_DESPL=VICTIMA_DESPL[!duplicated(VICTIMA_DESPL$A01),-4]

VICTIMA_DESPL=VICTIMA_DESPL %>% group_by(CodigoMunicipio, Periodo) %>% summarise(VICTIMA=sum(VICTIMAS,na.rm = TRUE))

DATA=Reduce(function(x,y) merge(x = x, y = y, by = c("CodigoMunicipio","Periodo"), all.x=TRUE), list(compilado,MUJERJEFE,PERFIL_HOG,TENENCIA,TIPO_VIVIENDA,ZONA,IPM,LP,VICTIMA_DESPL,GRUPOSETINICOS,PIRAMIDES,SEXO,ESTADOHOGAR))
rm(list=ls()[!ls() %in% c("UNIDOS_2018","UNIDOS_2019","UNIDOS_2018_2019","DATA")])

firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

colnames(DATA)[grep("_sum",names(DATA))]=firstup(gsub("_sum","",grep("_sum",names(DATA),value = TRUE)))

DATA=merge(DATA,UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$CodigoMunicipio),c("Departamento","Municipio","CodigoMunicipio")],by="CodigoMunicipio", all.x=TRUE)
DATA=DATA[c(62:63,1:61)]

#Estado de logros por hogar agregado municipal
df_Municipal=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),c("Periodo","Departamento","Municipio","CodigoMunicipio",grep("logro",names(UNIDOS_2018_2019),value = TRUE)[-(29:56)])]  %>% gather(key = "LOGRO",value = "ESTADO",grep("logro",names(UNIDOS_2018_2019),value = TRUE)[-(29:56)]) %>% group_by(Periodo,Departamento,Municipio,CodigoMunicipio, LOGRO, ESTADO) %>% count()
df_Municipal=df_Municipal[df_Municipal$LOGRO %in% c("logro09","logro10","logro11","logro15","logro21","logro22","logro23","logro24","logro26"),]
colnames(df_Municipal)[7]="CANTIDAD"

#Estado de logros por integrante agregado municipal
df_1_Municipal=UNIDOS_2018_2019[c("Periodo","Departamento","Municipio","CodigoMunicipio",grep("_I",names(UNIDOS_2018_2019),value = TRUE))]  %>% gather(key = "LOGRO",value = "ESTADO",grep("_I",names(UNIDOS_2018_2019),value = TRUE)) %>% group_by(Periodo,Departamento,Municipio,CodigoMunicipio, LOGRO, ESTADO) %>% count()
df_1_Municipal=df_1_Municipal[!(df_1_Municipal$LOGRO %in% paste(c("logro09","logro10","logro11","logro15","logro21","logro22","logro23","logro24","logro26"),"_I",sep = "")),]
colnames(df_1_Municipal)[7]="CANTIDAD"

setwd("~/Datos/2020/Tablero de control")
write.csv2(DATA, file = "UNIDOS_HOGARES_MUNICIPAL_06022020.csv", row.names = FALSE)
write.csv2(df_Municipal, file = "MUNICIPAL_ESTADO_DE_LOGROS_07022020.csv", row.names = FALSE)
write.csv2(df_1_Municipal, file = "MUNICIPAL_ESTADO_DE_LOGROS_I_07022020.csv", row.names = FALSE)

##################
#ESTADO DE LOGROS#
##################
DATA_1=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),c("A01","Departamento","Municipio","CodigoMunicipio","A04",grep("denominacion|logro",names(UNIDOS_2019), value = TRUE))]
DATA_1=DATA_1[c(1:33,62:63)]

DATA_2=UNIDOS_2018_2019[!duplicated(UNIDOS_2018_2019$A01),c("Longitud","Latitud","TOTALINTEGRANTES","MUJERJEFE","PERFIL_HOG",grep("^A0|^B|^C|^D0",names(UNIDOS_2018_2019), value = TRUE))]
DATA_2=DATA_2[c(-85,-86)]
DATA_2= DATA_2 %>% group_by(A01) %>% mutate(Integrantes_promedio_perfil=round(sum()))
DATA_2=Reduce(function(x,y) merge(x = x, y = y, by = "A01", all.x=TRUE), list(DATA_2, SEXO, PIRAMIDES,GRUPOSETINICOS))

DATA_3=UNIDOS_2018_2019[c("A01","Discapacidad")] %>% group_by(A01) %>% mutate(DISCAPACIDAD=ifelse(any(Discapacidad==1),1,0))
DATA_3=DATA_3 %>% group_by(A01) %>% summarise(DISCAPACIDAD_Num=sum(Discapacidad),DISCAPACIDAD_Cat=max(DISCAPACIDAD))

DATA_4=UNIDOS_2018_2019[c("A01","VICTIMA_DESPL")] %>% group_by(A01) %>% mutate(VICTIMAS=ifelse(any(VICTIMA_DESPL==1),1,0))
DATA_4=DATA_4 %>% group_by(A01) %>% summarise(VICTIMAS_Num=sum(VICTIMA_DESPL),VICTIMAS_Cat=max(VICTIMAS))

DATA=merge(DATA_1,DATA_2[-8],by.x="idHogar",by.y="A01", all.x=T)
DATA=merge(DATA,DATA_3,by.x="idHogar",by.y="A01", all.x=T)
DATA=merge(DATA,DATA_4,by.x="idHogar",by.y="A01", all.x=T)

Error_coordenada_19112018 = read.csv("~/Datos/2019/EJERCICIOS/Visor/Piloto_2019/Error_coordenada_19112018.csv")
HOGAR_UNIDOS_SISBEN       = read.csv("~/Datos/2019/EJERCICIOS/Visor/Piloto_2019/HOGAR_UNIDOS_SISBEN.csv", sep="|")
SISBENIV                  = read.csv("~/Datos/2019/SISBEN IV/Datos/base_focalizados.csv")

DATA=merge(DATA,HOGAR_UNIDOS_SISBEN[c("idHogar","IdHogarFuente")],by="idHogar",all.x=TRUE)
DATA=merge(DATA,SISBENIV[c("cod_hogar",grep("coord",names(SISBENIV), value = TRUE))], by.x="idHogar", by.y="cod_hogar", all.x=TRUE)
DATA$Latitud=ifelse(DATA$idHogar %in% Error_coordenada_19112018$idHogar,DATA$coord_x_auto_enc ,DATA$Latitud)
DATA$Longitud=ifelse(DATA$idHogar %in% Error_coordenada_19112018$idHogar,DATA$coord_y_auto_enc ,DATA$Longitud)

DATA=DATA[c(-(116:119),-(141:150))]

Centroides_municipios_Colombia= read.csv2("~/Datos/2018/Shapefile/Centroides_municipios_Colombia.csv", encoding="UTF-8")
DATA=merge(DATA,Centroides_municipios_Colombia[c("MPIO_CCDGO","POINT_X","POINT_Y")],by.x="CodigoMunicipio",by.y="MPIO_CCDGO", all.x=TRUE)
DATA$Longitud=ifelse(DATA$idHogar %in% Error_coordenada_19112018$idHogar,DATA$POINT_X,DATA$Longitud)
DATA$Latitud=ifelse(DATA$idHogar %in% Error_coordenada_19112018$idHogar,DATA$POINT_Y,DATA$Latitud)

DATA=DATA[c(-(137:138))]


#Estado de logros por hogar
df=UNIDOS_2018_2019[c("A01","Departamento","Municipio","CodigoMunicipio",grep("logro",names(UNIDOS_2018_2019),value = TRUE))][-(33:60)]  %>% gather(key = "LOGRO",value = "ESTADO",grep("logro",names(UNIDOS_2018_2019),value = TRUE)[-(29:56)]) %>% group_by(A01,Departamento,Municipio,CodigoMunicipio, LOGRO, ESTADO) %>% count()
colnames(df)[7]="CANTIDAD"

#Estado de logros por integrante
df_1=UNIDOS_2019_LOGROS[UNIDOS_2019_LOGROS$idHogar %in% DATA$idHogar,c("idHogar","Departamento","Municipio","CodigoMunicipio",grep("_I",names(UNIDOS_2019_LOGROS),value = TRUE)[-(27:28)])]  %>% gather(key = "LOGRO",value = "ESTADO",grep("_I",names(UNIDOS_2019_LOGROS),value = TRUE)[-(27:28)]) %>% group_by(idHogar,Departamento,Municipio,CodigoMunicipio, LOGRO, ESTADO) %>% count()
colnames(df_1)[7]="CANTIDAD"

setwd("~/Datos/2019/EJERCICIOS/Visor/Piloto_2019")
write.csv2(df, file = "ESTADO_DE_LOGROS_07022020.csv", row.names = FALSE)
write.csv2(df_1, file = "ESTADO_DE_LOGROS_I_07022020.csv", row.names = FALSE)


#############

#VICTIMAS#
##########
VICTIMAS =VICTIMAS[c("TIPODOCUMENTO","DOCUMENTO","PRIMERNOMBRE","SEGUNDONOMBRE","PRIMERAPELLIDO","SEGUNDOAPELLIDO","FECHANACIMIENTO","HECHO")]
VICTIMAS_HechoDesplazamientoForzado  = VICTIMAS[VICTIMAS$HECHO=="Desplazamiento forzado",]

fonetico=function(text){
  text=gsub("¥|Ð","Y|D",text)
  text=str_replace_all(gsub("`|\\'", "", toupper(text)),"[[:punct:]]", "")
  text=str_replace_all(text,"[^[:graph:]]", " ")
  text=stri_trans_general(text,"Latin-ASCII")
  text=soundex(text, maxCodeLen = 4L)
  return(text)
}

fecha=function(text){
  text=strptime(text, format= "%Y-%m-%d")
  text=format(text, format="%Y/%m/%d")
  return(text)
}

fecha_1=function(text){
  text=strptime(text, format= "%d/%m/%Y")
  text=format(text, format="%Y/%m/%d")
  return(text)
}


VICTIMAS_HechoDesplazamientoForzado$TIPODOCUMENTO =recode(VICTIMAS_HechoDesplazamientoForzado$TIPODOCUMENTO, `NUIP` = "RC")

UNIDOS_2018_2019$VICTIMA=ifelse((UNIDOS_2018_2019$E05 %in% as.character(VICTIMAS_HechoDesplazamientoForzado$TIPODOCUMENTO)) & (UNIDOS_2018_2019$E06 %in% as.character(VICTIMAS_HechoDesplazamientoForzado$DOCUMENTO)),1,0)
UNIDOS_2018_2019$FONETIC1=ifelse(paste(fonetico(UNIDOS_2018_2019$E01_a),
                                       fonetico(UNIDOS_2018_2019$E01_b),
                                       fonetico(UNIDOS_2018_2019$E01_c),
                                       fonetico(UNIDOS_2018_2019$E01_d),
                                       fecha(UNIDOS_2018_2019$E02)) %in%
                                   paste(fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERNOMBRE),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$SEGUNDONOMBRE),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERAPELLIDO),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$SEGUNDOAPELLIDO),
                                         fecha_1(VICTIMAS_HechoDesplazamientoForzado$FECHANACIMIENTO)),1,0)

UNIDOS_2018_2019$FONETIC2=ifelse(paste(fonetico(UNIDOS_2018_2019$E01_a),
                                       fonetico(UNIDOS_2018_2019$E01_c),
                                       fonetico(UNIDOS_2018_2019$E01_d),
                                       fecha(UNIDOS_2018_2019$E02)) %in%
                                   paste(fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERNOMBRE),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERAPELLIDO),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$SEGUNDOAPELLIDO),
                                         fecha_1(VICTIMAS_HechoDesplazamientoForzado$FECHANACIMIENTO)),1,0)

UNIDOS_2018_2019$FONETIC3=ifelse(paste(fonetico(UNIDOS_2018_2019$E01_a),
                                       fonetico(UNIDOS_2018_2019$E01_c),
                                       fonetico(UNIDOS_2018_2019$E01_d),
                                       fecha(UNIDOS_2018_2019$E02)) %in%
                                   paste(fonetico(VICTIMAS_HechoDesplazamientoForzado$SEGUNDONOMBRE),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERAPELLIDO),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$SEGUNDOAPELLIDO),
                                         fecha_1(VICTIMAS_HechoDesplazamientoForzado$FECHANACIMIENTO)),1,0)

UNIDOS_2018_2019$FONETIC4=ifelse(paste(fonetico(UNIDOS_2018_2019$E01_b),
                                       fonetico(UNIDOS_2018_2019$E01_c),
                                       fonetico(UNIDOS_2018_2019$E01_d),
                                       fecha(UNIDOS_2018_2019$E02)) %in%
                                   paste(fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERNOMBRE),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERAPELLIDO),
                                         fonetico(VICTIMAS_HechoDesplazamientoForzado$SEGUNDOAPELLIDO),
                                         fecha_1(VICTIMAS_HechoDesplazamientoForzado$FECHANACIMIENTO)),1,0)

INTEGRANTES$FONETIC5=ifelse(paste(fonetico(INTEGRANTES$SEGUNDONOMBRE),
                                  fonetico(INTEGRANTES$PRIMERAPELLIDO),
                                  fonetico(INTEGRANTES$SEGUNDOAPELLIDO),
                                  fecha(INTEGRANTES$FECHANACIMIENTO)) %in%
                              paste(fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERNOMBRE),
                                    fonetico(VICTIMAS_HechoDesplazamientoForzado$PRIMERAPELLIDO),
                                    fonetico(VICTIMAS_HechoDesplazamientoForzado$SEGUNDOAPELLIDO),
                                    fecha(VICTIMAS_HechoDesplazamientoForzado$FECHANACIMIENTO)),1,0)



UNIDOS_2018_2019$VICTIMA_DESPL=ifelse(UNIDOS_2018_2019$VICTIMA==1 |
                                        UNIDOS_2018_2019$FONETIC1==1 |
                                        UNIDOS_2018_2019$FONETIC2==1 |
                                        UNIDOS_2018_2019$FONETIC3==1 |
                                        UNIDOS_2018_2019$FONETIC4==1 |
                                        UNIDOS_2018_2019$FONETIC5==1,1,0)

setwd("~/Datos/2019/EJERCICIOS/Visor/Piloto_2019")
write.csv2(UNIDOS_2018_2019, file = "UNIDOS_2018_2019_VISOR_05022020.csv", row.names = FALSE)

#UNIDOS_2018_2019 <- read.csv("~/Datos/2019/EJERCICIOS/Visor/Piloto_2019/UNIDOS_2018_2019_VISOR_19112018.csv", sep=";")

#Llave SISBEN - UNIDOS
SISBENIV=merge(SISBENIV,HOGAR_UNIDOS_SISBEN[c("idHogar","IdHogarFuente")],by.x="cod_hogar",by.y="IdHogarFuente",all.x=TRUE)

View(SISBENIV[(SISBENIV$idHogar %in% "10047722") & !duplicated(SISBENIV$idHogar),c("idHogar",grep("coord",names(SISBENIV), value = TRUE))])

