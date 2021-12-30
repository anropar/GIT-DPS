#####################
# 2. DASHBOARD ESRI #
#####################
Carpeta = dirname(rstudioapi::getSourceEditorContext()$path)#El ultimo slash o backslash no se debe incluir

slash = "/"

Entradas=paste(Carpeta,"1. Entradas", sep = slash)# Defina el escritorio de entrada donde están los archivos requeridos.
Salidas =paste(Carpeta,"2. Salidas", sep = slash)# Defina el escritorio de salida donde serán enviado los archivos generados.

#     LIBRERIAS   ####
setwd(Carpeta)
source("Librerias.R", encoding = "UTF-8")# Las librerias que se usaran

setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.
# Precargue
Precargue = read_delim("Unidos_Sabana_20211027.txt",
                       "|", escape_double = FALSE, col_types = cols(Longitud = col_character(),
                                                                    Latitud = col_character(),
                                                                    Altitud = col_character(),
                                                                    E09 = col_character(),
                                                                    FechaInicio = col_date(format = "%Y-%m-%d"),
                                                                    E02 = col_date(format = "%Y-%m-%d"),
                                                                    E10 = col_date(format = "%Y-%m-%d"),
                                                                    C01 = col_number(),
                                                                    C02 = col_number(),
                                                                    C03 = col_number(),
                                                                    C04 = col_number(),
                                                                    C10_1 = col_number(),
                                                                    C11_1 = col_number(),
                                                                    D02 = col_number(),
                                                                    E02_1 = col_number(),
                                                                    G06 = col_number(),
                                                                    I04_1 = col_number(),
                                                                    J04 = col_number(),
                                                                    J12 = col_number(),
                                                                    J13 = col_number(),
                                                                    J13_1 = col_number()), locale = locale(grouping_mark = ",", encoding = "ISO-8859-1"), trim_ws = TRUE)


#################
# 2.1. Marcas #
#################
DATA = Precargue[c("A01","IdIntegrante","A03_1","A04","B01","EdadCargue","E02","E01_1","E01_2","E01_3","E01_4","E03","E08","E09","E14","D01", grep("F01", names(Precargue), value = T)[-8])]
DATA$E02_1 = DATA$EdadCargue

setwd(Entradas)
BaseGestion_2021 = read_delim("BaseGestion Hogares Acomp Diciembre 2021.txt", ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
DATA = DATA[DATA$IdIntegrante %in% BaseGestion_2021$idIntegranteHogar,]
DATA = merge(DATA, BaseGestion_2021[c("idIntegranteHogar","denominacionIPM","EstadoHogar")], by.x = "IdIntegrante", by.y = "idIntegranteHogar")

setwd(Carpeta)
source("Perfiles.R", encoding = "UTF-8")
source("Ciclo vital.R", encoding = "UTF-8")
source("Mujer jefe.R", encoding = "UTF-8")

DATA = Perfiles(DATA, A01, E02_1)# Genera la columna de perfil del hogar
DATA = Ciclo(DATA, E02_1)# Genera la columna de ciclo vital
DATA = Mujer_jefe(DATA, E03, E14)# Genera la columna de ciclo vital

#################
# 2.2. Victimas #
#################
setwd(Carpeta)
source("Victimas.R", encoding = "UTF-8")# Genera las marcas de victimas

###############################
# 10. Frecuencias municipales #
###############################
setwd(Carpeta)
source("Frecuencias municipales.R", encoding = "UTF-8")# Genera el archivo de frecuencias municipales
