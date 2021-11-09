################
#CARGA DE DATOS#
################

setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.

########################
# Importacion de datos #
########################
tictoc::tic("Total")

data.files = list.files(pattern = "*.xlsm")
data.files = grep("PlantillaRegistrosAdministrativos_20211008", data.files, value = T, invert = T)

DATA <- do.call(rbind, lapply(data.files, function(x) cbind(read_excel(x, sheet = "Plantilla"), Archivo=strsplit(x,'\\.')[[1]][1])))

DATA = DATA %>% drop_na(`NUMERO DOCUMENTO`)

Precargue = read_delim("Unidos_Sabana_20211027.txt",
                       "|", escape_double = FALSE, col_types = cols(Longitud = col_character(),
                                                                    Latitud = col_character(),
                                                                    Altitud = col_character(),
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

Oferta = read_excel("Oferta disponible 20211109.xlsx")
library(stringr)
Oferta$`Cód Municipio`=str_pad(Oferta$`Cód Municipio`, 5, pad = "0")

DATA$DEPARTAMENTO = ifelse(is.na(DATA$DEPARTAMENTO) & DATA$`NUMERO DOCUMENTO` %in% Precargue$E09, Precargue$A02, DATA$DEPARTAMENTO) 
DATA$`CODIGO DEPARTAMENTO DANE` = ifelse(is.na(DATA$`CODIGO DEPARTAMENTO DANE`) & DATA$`NUMERO DOCUMENTO` %in% Precargue$E09, Precargue$A02_1, DATA$`CODIGO DEPARTAMENTO DANE`) 
DATA$MUNICIPIO = ifelse(is.na(DATA$MUNICIPIO) & DATA$`NUMERO DOCUMENTO` %in% Precargue$E09, Precargue$A03, DATA$MUNICIPIO) 
DATA$`CODIGO MUNICIPIO DANE` = ifelse(is.na(DATA$`CODIGO MUNICIPIO DANE`) & DATA$`NUMERO DOCUMENTO` %in% Precargue$E09, Precargue$A03_1, DATA$`CODIGO MUNICIPIO DANE`) 
DATA$`FECHA DE NACIMIENTO` = ifelse(is.na(DATA$`FECHA DE NACIMIENTO`) & DATA$`NUMERO DOCUMENTO` %in% Precargue$E09, Precargue$E02, DATA$`FECHA DE NACIMIENTO`) 
DATA$SEXO = ifelse(is.na(DATA$SEXO) & DATA$`NUMERO DOCUMENTO` %in% Precargue$E09, Precargue$E03, DATA$SEXO) 
DATA$`TIPO DOCUMENTO` = ifelse(is.na(DATA$`TIPO DOCUMENTO`) & DATA$`NUMERO DOCUMENTO` %in% Precargue$E09, Precargue$E08, DATA$`TIPO DOCUMENTO`) 

DATA$`LOGRO y/o PRIVACIÓN GESTIONADA` = ifelse(is.na(DATA$`LOGRO y/o PRIVACIÓN GESTIONADA`) & paste(DATA$`ID OFERTA`, DATA$`CODIGO MUNICIPIO DANE`) %in% paste(Oferta$`ID Oferta`, Oferta$`Cód Municipio`), Oferta$`Logro Asociado`, DATA$`LOGRO y/o PRIVACIÓN GESTIONADA`) 

DATA$`TIPO DOCUMENTO` = recode(DATA$`TIPO DOCUMENTO`,  `1` = "Registro Civil", `2` = "Tarjeta de Identidad",
                               `3` = "Cédula de Ciudadanía", `4` = "Cédula de Extranjería",
                               `5` = "Documento Nacional de Identidad (DNI) del país de origen", `6` = "Pasaporte",
                               `7` = "Salvoconducto para refugiado", `8` = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos",
                               "RC" = "Registro Civil", "REGISTRO CIVIL" = "Registro Civil",
                               "CE" = "Cédula de Extranjería", "CvÉ¬âDULA DE EXTRANJERvÉ¬çA" = "Cédula de Extranjería",
                               "DNI" = "Documento Nacional de Identidad (DNI) del país de origen", "PAS" = "Pasaporte",
                               "Tarjeta de identidad" = "Tarjeta de Identidad", "TI" = "Tarjeta de Identidad", "TARJETA DE IDENTIDAD" = "Tarjeta de Identidad",
                               "SAL" = "Salvoconducto para refugiado", "PEP" = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos",
                               "Cédula de ciudadanía" = "Cédula de Ciudadanía")

DATA$SEXO = recode(DATA$SEXO, `1` = "Hombre", `2` = "Mujer")


tictoc::toc()

# No van a estar los A01 y IdIntegrante
# Sin información en: DEPARTAMENTO, CODIGO DEPARTAMENTO DANE, MUNICIPIO, CODIGO MUNICIPIO DANE, FECHA DE NACIMIENTO y SEXO. Se obtienen del precargue con cruce fonetico y documento
# LOGRO y/o PRIVACIÓN GESTIONADA se obtiene de la oferta disponible y la llave es el ID OFERTA. 
