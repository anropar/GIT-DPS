

#

library(readr)

setwd("~/Datos/Muestras")

n=20

# 2015
Hogares_2015 = read_delim("~/Documents/DPS/Datos/2015/Hogares_2015.txt", "|", escape_double = FALSE, trim_ws = TRUE)
Integrantes_2015 = read_delim("~/Documents/DPS/Datos/2015/Integrantes_2015.txt", "|", escape_double = FALSE, trim_ws = TRUE)

Hogares_2015 = Hogares_2015[Hogares_2015$FOLIO %in% intersect(Hogares_2015$FOLIO, Integrantes_2015$FOLIO),]
Integrantes_2015 = Integrantes_2015[Integrantes_2015$FOLIO %in% intersect(Integrantes_2015$FOLIO, Hogares_2015$FOLIO),]

ID = sample(unique(Hogares_2015$FOLIO), n)

Hogares_2015 = Hogares_2015[Hogares_2015$FOLIO %in% ID,]
Integrantes_2015 = Integrantes_2015[Integrantes_2015$FOLIO %in% ID,]

library(openxlsx)
write.csv(Hogares_2015, file = "Sabana_Hogares_2015.csv", row.names = FALSE)
write.csv(Integrantes_2015, file = "Sabana_Integrantes_2015.csv", row.names = FALSE)

# 2016-2017
Unidos_2016_2018 = read_delim("~/Datos/2018/Unidos_Gestion_2016_2018_Certificada.csv", 
                                "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                trim_ws = TRUE)

Sabana_2016_2017 = Unidos_2016_2018[Unidos_2016_2018$MARCA %in% "2016-2017",]

ID = sample(unique(Sabana_2016_2017$HOGAR), n)

Sabana_2016_2017 = Sabana_2016_2017[Sabana_2016_2017$HOGAR %in% ID,]

Campos = c("HOGAR","IDINTEGRANTE","DEPARTAMENTO","MUNICIPIO","PRIMERNOMBRE","SEGUNDONOMBRE","PRIMERAPELLIDO","SEGUNDOAPELLIDO","TIPODOCUMENTO","NUMERODOCUMENTO","FECHANACIMIENTO")
library(openxlsx)
setwd("~/Datos/Muestras")
write.csv(Sabana_2016_2017[Campos], file = paste("Sabana_2016_2017","_Muestra_",n,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

#2018
Sabana_2018 = Unidos_2016_2018[Unidos_2016_2018$MARCA %in% "2018",]

ID = sample(unique(Sabana_2018$HOGAR), n)

Sabana_2018 = Sabana_2018[Sabana_2018$HOGAR %in% ID,]

library(openxlsx)
Campos = c("HOGAR","IDINTEGRANTE","DEPARTAMENTO","MUNICIPIO","PRIMERNOMBRE","SEGUNDONOMBRE","PRIMERAPELLIDO","SEGUNDOAPELLIDO","TIPODOCUMENTO","NUMERODOCUMENTO","FECHANACIMIENTO")
setwd("~/Datos/Muestras")
write.csv(Sabana_2018[Campos], file = paste("Sabana_2018","_Muestra_",n,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

#2019
Sabana_Caracterizacion_2019 <- read_delim("~/Datos/2019/Sabana_Caracterizacion_2019.txt", 
                                         "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                          trim_ws = TRUE)

ID = sample(unique(Sabana_Caracterizacion_2019$A01), n)

Sabana_Caracterizacion_2019 = Sabana_Caracterizacion_2019[Sabana_Caracterizacion_2019$A01 %in% ID,]

library(openxlsx)
Campos = c("A01","idIntegranteHogar","A02","A03","E01_a","E01_b","E01_c","E01_d","E05","E06","E02")
setwd("~/Datos/Muestras")
write.csv(Sabana_Caracterizacion_2019[Campos], file = paste("Sabana_2019","_Muestra_",n,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)
# 
# library(openxlsx)
# write.csv(Campos_basicos, file = "Campos_basicos_25_03_2021.csv", row.names = FALSE)
# write.csv(Campos_basicos, file = "Campos_basicos_25_03_2021.csv", row.names = FALSE)
# write.csv(Campos_basicos, file = "Campos_basicos_25_03_2021.csv", row.names = FALSE)

#2021
Sabana_PuertoLopez_20210318 = read_delim("Datos/Sabana_PuertoLopez_20210318.txt","|", escape_double = FALSE, trim_ws = TRUE)

ID = sample(unique(Sabana_PuertoLopez_20210318$A01), n)

Sabana_PuertoLopez_20210318 = Sabana_PuertoLopez_20210318[Sabana_PuertoLopez_20210318$A01 %in% ID,]

library(openxlsx)
Campos = c("A01","IdIntegrante","A02","A02_1","A03_1","A03","E01_A","E01_B","E01_C","E01_D","E05","E06","E02")
setwd("~/Datos/Muestras")
write.csv(Sabana_PuertoLopez_20210318[Campos], file = paste("Sabana_PuertoLopez","_Muestra_",n,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

#Sisben IV
setwd("~/GitHub/GIT-DPS/SISBEN IV")
source("Importacion de datos.R", encoding = "UTF-8")

ID = sample(unique(DATA$A01), n)

DATA = DATA[DATA$A01 %in% ID,]

library(openxlsx)
Campos = c("A01","IdIntegrante","A02","A02_1","A03_1","A03","E01_1","E01_2","E01_3","E01_4","E08","E09","E02")
setwd("~/Datos/Muestras")
write.csv(DATA[Campos], file = paste("Sisben_IV","_Muestra_",n,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)