################
#CARGA DE DATOS#
################

setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.

#####################
# ESTRATEGIA UNIDOS #
#####################
tictoc::tic("Total")

DATA = read_delim("Unidos_Sabana_20211027.txt",
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

# DATA = DATA %>% select(-EdadCargue)
#
# DATA = DATA %>% select(-E02_1)
# setnames(DATA, old = "EdadCargue", new = "E02_1")
#
#Si se necesita una restriccion de fechas
#DATA=DATA[DATA$FechaInicio>="2021-08-26",]

#############
# SISBEN IV #
#############

# DATA = read_delim("Unidos_Sabana_2021.csv", "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),  trim_ws = TRUE)
#
# Campos_DATA = read_excel("Campos_Unidos_2015_2021.xlsx")
# Campos_DATA$LOGROS = apply(Campos_DATA[grep("LOGRO", names(Campos_DATA), value = T)], 1, function(i){ paste(na.omit(i), sep = ",", collapse = ",") })
# Campos_DATA = Campos_DATA[!is.na(Campos_DATA$`SISBEN IV`) & !is.na(Campos_DATA$`UNIDOS 2020`), c(1:8,46)]
# Campos_DATA = Campos_DATA[!is.na(Campos_DATA$`UNIDOS 2020`), c(1:8,46)]
#
# Campos_DATA = Campos_DATA[!Campos_DATA$LOGROS=="",]
#
# DATA = DATA[c("A01","IdIntegrante","A02_1",Campos_DATA$`UNIDOS 2020`)]
# 
# DATA = DATA[Campos_DATA$`UNIDOS 2020`]

rm(Campos_DATA)

############################
# SISBEN IV  PRIMER VERSION#
############################

# # SISBENIV = read_delim("Focalizacion_Unidos.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# # SISBENIV = SISBENIV[c("ID_UG","ID_PER","EDAD_CALCULADA")]
# IDS = read_delim("Hogares_Unidos_Sisben 2.txt", ";", escape_double = FALSE, trim_ws = TRUE)
#
# setnames(IDS, old = c("IdHogar"), new = c("A01"), skip_absent = T)
#
# DATA = merge(DATA, IDS %>% select(-A01), by=c("IdIntegrante"), all.x = T)
# rm(IDS)
#
# # DATA = merge(DATA %>% select(-E02_1), DATA %>% select(-c("ID_UG","ID_PER")), by = c("A01","IdIntegrante"))
# # rm(SISBENIV)
#
# ###########################
# # SISBEN IV  OTRAS VERSION#
# ###########################
#
#
# setnames(DATA, old = names(DATA), new = tolower(names(DATA)))
#
# # Provicionalmente se definieron como identificadores la concatenación de campos contenidos en la base
# DATA$A01 = paste(DATA$ide_ficha_origen, str_pad(DATA$ide_edificacion, 2, pad = "0"), str_pad(DATA$ide_hogar, 2, pad = "0"), sep = "-")
# DATA$IdIntegrante = paste(DATA$ide_ficha_origen, str_pad(DATA$ide_edificacion, 2, pad = "0"), str_pad(DATA$ide_hogar, 2, pad = "0"), str_pad(DATA$ide_persona, 2, pad = "0"), sep = "-")
#
# # Renombra masivamente
# setnames(DATA, old = Campos_DATA$`SISBEN IV`, new = Campos_DATA$`UNIDOS 2020`, skip_absent = T)
#
# setwd(Carpeta)
# source("Adaptacion SISBEN IV.R", encoding = "UTF-8")# Se recodifican valores de Sisben IV para ajustarsen a DATA: 9 y 0 a 99
#
# # campos de departamentos y municipios
# MUNICIPIOS = read_excel("~/Documents/DPS/Consultas/2020/MUNICIPIOS.xlsx")
# setnames(MUNICIPIOS, old = c("CODIGO_MUNICIPIO","DEPARTAMENTO","MUNICIPIO"), new = c("A03_1","A02","A03"))
#
# DATA = merge(DATA, MUNICIPIOS[!duplicated(MUNICIPIOS$A03_1),], by = "A03_1", all.x = T)
#
# rm(MUNICIPIOS)

tictoc::toc()
