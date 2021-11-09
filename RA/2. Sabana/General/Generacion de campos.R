# Generación de campos

# DATA$EdadActual = as.integer(age_calc(as.Date(DATA$E02, format = "%d/%m/%Y"), enddate = as.Date(DATA$FechaInicio, format = "%d/%m/%Y"), units = "years", precise = TRUE))

DATA = DATA %>% group_by(A01) %>% mutate(Total_Personas = n())

# DATA$`TIPO DOCUMENTO` = as.numeric(as.character(recode_factor(DATA$`TIPO DOCUMENTO`, `Registro Civil` = 1,
#                                                               `Tarjeta de Identidad` = 2, `Cédula de Ciudadanía` = 3,
#                                                               `Cédula de Extranjería` = 4, `Documento Nacional de Identidad (DNI) del país de origen` = 5,
#                                                               `Pasaporte` = 6,
#                                                               `Salvoconducto para refugiado` = 7,
#                                                               `Permiso especial de permanencia (PEP) para ciudadanos venezolanos` = 8)))

DATA$Cruce = ifelse(DATA$`NUMERO DOCUMENTO` %in% Precargue$E09,1,0)

# DATA$`TIPO DOCUMENTO` = as.character(recode_factor(DATA$`TIPO DOCUMENTO`, `1` = "Registro Civil",
#                                                    `2` = "Tarjeta de Identidad", `3` = "Cédula de Ciudadanía",
#                                                    `4` = "Cédula de Extranjería", `5` = "Documento Nacional de Identidad (DNI) del país de origen",
#                                                    `6` = "Pasaporte", `7` = "Salvoconducto para refugiado",
#                                                    `8` = "Permiso especial de permanencia (PEP) para ciudadanos venezolanos"))

DATA$Duplicados = ifelse(duplicated(DATA$`NUMERO DOCUMENTO`),1,0)
  