################
#CARGA DE DATOS#
################

setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.

########################
# Importacion de datos #
########################
tictoc::tic("Total")

data.files = list.files(pattern = "*.xlsm")

DATA <- do.call(rbind, lapply(data.files, function(x) cbind(read_excel(x, sheet = "Plantilla"), Archivo=strsplit(x,'\\.')[[1]][1])))

DATA = DATA %>% drop_na(`NUMERO DOCUMENTO`)

Campos = names(DATA)

DATA = DATA %>% select(-c("DEPARTAMENTO","CODIGO DEPARTAMENTO DANE","MUNICIPIO","CODIGO MUNICIPIO DANE","LOGRO y/o PRIVACIÓN GESTIONADA","FECHA DE NACIMIENTO","SEXO"))

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

Oferta = read_excel("Oferta disponible 20211103.xlsx")

setnames(Oferta, old = "Logro Asociado", new = "LOGRO y/o PRIVACIÓN GESTIONADA")

# DATA$`TIPO DOCUMENTO` = as.numeric(as.character(recode_factor(DATA$`TIPO DOCUMENTO`, `Registro Civil` = 1,
#                                                               `Tarjeta de Identidad` = 2, `Cédula de Ciudadanía` = 3,
#                                                               `Cédula de Extranjería` = 4, `Documento Nacional de Identidad (DNI) del país de origen` = 5,
#                                                               `Pasaporte` = 6,
#                                                               `Salvoconducto para refugiado` = 7,
#                                                               `Permiso especial de permanencia (PEP) para ciudadanos venezolanos` = 8)))

# Merge
DATA = merge(DATA, Precargue[c("A01","IdIntegrante","A02","A02_1","A03","A03_1","E02","E03","E08","E09")], by.x = c("NUMERO DOCUMENTO"), by.y = c("E09"), all.x = T)

setnames(DATA, old = c("A02","A02_1","A03","A03_1","E02","E03"), new = c("DEPARTAMENTO","CODIGO DEPARTAMENTO DANE","MUNICIPIO","CODIGO MUNICIPIO DANE","FECHA DE NACIMIENTO","SEXO"))

library(stringr)
Oferta$`Cód Municipio`=str_pad(Oferta$`Cód Municipio`, 5, pad = "0")

DATA = merge(DATA, Oferta[c("Cód Municipio","ID Oferta","LOGRO y/o PRIVACIÓN GESTIONADA")], by.x = c("ID OFERTA","CODIGO MUNICIPIO DANE"), by.y = c("ID Oferta","Cód Municipio"), all.x = T)

DATA = DATA[c(Campos, "A01", "IdIntegrante")]

# DATA$`TIPO DOCUMENTO` = as.character(recode_factor(DATA$`TIPO DOCUMENTO`, `1` = "Registro Civil",
#                                                    `2` = "Tarjeta de Identidad", `3` = "Cédula de Ciudadanía",
#                                                    `4` = "Cédula de Extranjería", `5` = "Documento Nacional de Identidad (DNI) del país de origen",
#                                                    `6` = "Pasaporte", `7` = "Salvoconducto para refugiado",
#                                                    `8` = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos"))

DATA$MUNICIPIO = paste(DATA$`CODIGO MUNICIPIO DANE`, DATA$MUNICIPIO, sep = "-")
  
tictoc::toc()

# No van a estar los A01 y IdIntegrante
# Sin información en: DEPARTAMENTO, CODIGO DEPARTAMENTO DANE, MUNICIPIO, CODIGO MUNICIPIO DANE, FECHA DE NACIMIENTO y SEXO. Se obtienen del precargue con cruce fonetico y documento
# LOGRO y/o PRIVACIÓN GESTIONADA se obtiene de la oferta disponible y la llave es el ID OFERTA. 
