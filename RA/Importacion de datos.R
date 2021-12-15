################
#CARGA DE DATOS#
################

setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.

########################
# Importacion de datos #
########################
tictoc::tic("Total")

# Precargue
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

# Oferta
Oferta = read_excel("Oferta disponible 20211125.xlsx")
library(stringr)
Oferta$`Cód Municipio` = str_pad(Oferta$`Cód Municipio`, 5, pad = "0")

# Registros administrativos
setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path),"1. Entradas","Entrega 1", sep = slash))

data.files = list.files(pattern = "*.xlsm")
data.files = grep("PlantillaRegistrosAdministrativos_20211008", data.files, value = T, invert = T)

Original = do.call(rbind, lapply(data.files, function(x) cbind(read_excel(x, sheet = "Plantilla", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "text", "text", 
                                                                                                                "text", "text", "text", "date", "text", "date", "text")), Archivo=strsplit(x,'\\.')[[1]][1])))
Original = Original  %>%  filter(if_any(names(Original[-17]), ~ !is.na(.x)))

setwd(paste(Carpeta,"Consultas", sep = slash))
source("Limpiador de textos.R")
Original$`PRIMER NOMBRE` = limpiador_texto(Original$`PRIMER NOMBRE`)
Original$`SEGUNDO NOMBRE` = limpiador_texto(Original$`SEGUNDO NOMBRE`)
Original$`PRIMER APELLIDO` = limpiador_texto(Original$`PRIMER APELLIDO`)
Original$`SEGUNDO APELLIDO` = limpiador_texto(Original$`SEGUNDO APELLIDO`)

###########################
# Recodificacion de datos #
###########################

Original$`TIPO DOCUMENTO` = recode(Original$`TIPO DOCUMENTO`,  `1` = "Registro Civil", `2` = "Tarjeta de Identidad",
                                   `3` = "Cédula de Ciudadanía", `4` = "Cédula de Extranjería",
                                   `5` = "Documento Nacional de Identidad (DNI) del país de origen", `6` = "Pasaporte",
                                   `7` = "Salvoconducto para refugiado", `8` = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos",
                                   "RC" = "Registro Civil", "REGISTRO CIVIL" = "Registro Civil",
                                   "CE" = "Cédula de Extranjería", "CvÉ¬âDULA DE EXTRANJERvÉ¬çA" = "Cédula de Extranjería",
                                   "DNI" = "Documento Nacional de Identidad (DNI) del país de origen", "PAS" = "Pasaporte",
                                   "Tarjeta de identidad" = "Tarjeta de Identidad", "TI" = "Tarjeta de Identidad", "TARJETA DE IDENTIDAD" = "Tarjeta de Identidad",
                                   "SAL" = "Salvoconducto para refugiado", "PEP" = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos",
                                   "Cédula de ciudadanía" = "Cédula de Ciudadanía")

Original$`TIPO DOCUMENTO` = as.numeric(as.character(recode_factor(Original$`TIPO DOCUMENTO`, `Registro Civil` = 1,
                                                                  `Tarjeta de Identidad` = 2, `Cédula de Ciudadanía` = 3,
                                                                  `Cédula de Extranjería` = 4, `Documento Nacional de Identidad (DNI) del país de origen` = 5,
                                                                  `Pasaporte` = 6,
                                                                  `Salvoconducto para refugiado` = 7,
                                                                  `Permiso especial de permanencia (PEP) para ciudadanos venezolanos` = 8)))

Original$`ID OFERTA`[Original$`ID OFERTA`==71970 ] <- 71974#Solo aplica para la primera entrega

Original = dplyr::mutate(Original, ID = row_number()) %>% arrange(`NUMERO DOCUMENTO`)

#########################
#Generacion de variables#
#########################

#Las variables generadas se requieren para calculos posteriores.
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Generacion de campos.R", encoding = "UTF-8")

# DATA = Original %>% drop_na(`NUMERO DOCUMENTO`)

Original$`FECHA DE NACIMIENTO` = as.Date(Original$`FECHA DE NACIMIENTO`, format='%Y-%m-%d')

Original$DEPARTAMENTO = Original$A02
Original$`CODIGO DEPARTAMENTO DANE` = Original$A02_1
Original$MUNICIPIO = Original$A03
Original$`CODIGO MUNICIPIO DANE` = Original$A03_1
Original$`FECHA DE NACIMIENTO` = ifelse(is.na(Original$`FECHA DE NACIMIENTO`), Original$E02, Original$`FECHA DE NACIMIENTO`)
Original$SEXO = ifelse(is.na(Original$SEXO), Original$E03, Original$SEXO)
Original$`TIPO DOCUMENTO` = ifelse(is.na(Original$`TIPO DOCUMENTO`), Original$E08, Original$`TIPO DOCUMENTO`)

Original = merge(Original, Oferta[c("ID Oferta","Cód Municipio","Logro Asociado")], by.x = c("ID OFERTA","CODIGO MUNICIPIO DANE"), by.y = c("ID Oferta","Cód Municipio"), all.x = T)

Original$`LOGRO y/o PRIVACIÓN GESTIONADA` = ifelse(is.na(Original$`Logro Asociado`), Original$`LOGRO y/o PRIVACIÓN GESTIONADA`, Original$`Logro Asociado`) 
Original$`LOGRO y/o PRIVACIÓN GESTIONADA` = toupper(Original$`LOGRO y/o PRIVACIÓN GESTIONADA`)
Original$`ESTADO ACCESO A OFERTA` = toupper(Original$`ESTADO ACCESO A OFERTA`)

Original$SEXO = recode(Original$SEXO, `1` = "Hombre", `2` = "Mujer")
Original$SEXO = toupper(Original$SEXO)

Original$`FECHA DE NACIMIENTO` = as.Date(as.integer(Original$`FECHA DE NACIMIENTO`), origin = "1970-01-01")

# Original$`CODIGO DEPARTAMENTO DANE` = str_pad(Original$`CODIGO DEPARTAMENTO DANE`, 2, pad = "0")
# 
# MUNICIPIOS = read_excel("~/Datos/2020/REQUERIMIENTOS/Generacion de cortes/Entradas/MUNICIPIOS.xlsx", sheet = "Departamentos", skip = 8)
# MUNICIPIOS = MUNICIPIOS[1:33,]
# 
# Original = merge(Original, MUNICIPIOS, by.x = "CODIGO DEPARTAMENTO DANE", by.y = "Código", all.x = T)
# Original$DEPARTAMENTO = Original$Nombre
# Original = Original %>% select(-Nombre)
# 
# No van a estar los A01 y IdIntegrante
# Sin información en: DEPARTAMENTO, CODIGO DEPARTAMENTO DANE, MUNICIPIO, CODIGO MUNICIPIO DANE, FECHA DE NACIMIENTO y SEXO. Se obtienen del precargue con cruce fonetico y documento
# LOGRO y/o PRIVACIÓN GESTIONADA se obtiene de la oferta disponible y la llave es el ID OFERTA. 

patterns = c("ACTIVIDAD PRODUCTIVA", "AFILIACIÓN A SALUD", "EDUCACIÓN INICIAL",
             "ESCOLARIZACIÓN","NO TRABAJO INFANTIL","ACCESO A AGUA","SANEAMIENTO BÁSICO",
             "LEER Y ESCRIBIR","ESTUDIOS POSTSECUNDARIOS", "NO PISOS EN TIERRA",
             "PAREDES ADECUADAS","NO HACINAMIENTO","ACTIVIDAD PRODUCTIVA","FAMILIAS EN ACCIÓN")

Original$List_Logros = ifelse(grepl(paste(patterns, collapse="|"), toupper(Original$`LOGRO y/o PRIVACIÓN GESTIONADA`)),1,0)

tictoc::toc()