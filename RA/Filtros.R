
# Filtro de los registros adminitrativos por las reglas:
# 1. Cruzar el Numero de documento con la base precargue.
# 2. No estar duplicado por numero de documento y logro.
# 3. Contener los logros definidos en la compilación patterns.

patterns <- c("ACTIVIDAD PRODUCTIVA", "AFILIACIÓN A SALUD", "EDUCACIÓN INICIAL",
              "ESCOLARIDAD","NO TRABAJO INFANTIL","ACCESO A AGUA","SANEAMIENTO BÁSICO",
              "LEER Y ESCRIBIR","ESTUDIOS POSTSECUNDARIOS", "NO PISOS EN TIERRA",
              "PAREDES ADECUADAS","NO HACINAMIENTO","ACTIVIDAD PRODUCTIVA","FAMILIAS EN ACCIÓN")

# Consulta ####
Consulta=DATA[DATA$Cruce %in% 1 & DATA$Duplicados_Logro %in% 0 & 
                grepl(paste(patterns, collapse="|"), toupper(DATA$`LOGRO y/o PRIVACIÓN GESTIONADA`)),]

# Consulta=Consulta[!paste(Consulta$`NUMERO DOCUMENTO`,Consulta$`LOGRO y/o PRIVACIÓN GESTIONADA`) %in% paste(Primer_Cargue_Registros_Administrativos$`NUMERO DOCUMENTO`, Primer_Cargue_Registros_Administrativos$`LOGRO y/o PRIVACIÓN GESTIONADA`),]
# Consulta=DATA[DATA$Cruce %in% 1,]
# Consulta=DATA[DATA$Cruce %in% 1 & DATA$Duplicados_Logro %in% 0,]

# Duplicados ####

Duplicados_Precargue = Precargue[duplicated(Precargue$E09) | duplicated(Precargue$E09,fromLast = T),c("A01","IdIntegrante","E08","E09","E01_1","E01_2","E01_3","E01_4","A03_1")]



Duplicados_Consulta = Consulta[duplicated(paste(Consulta$`NUMERO DOCUMENTO`, Consulta$`CODIGO MUNICIPIO DANE`)) |
                                 duplicated(paste(Consulta$`NUMERO DOCUMENTO`, Consulta$`CODIGO MUNICIPIO DANE`), fromLast = T),]

DATA$Dist_Nombres = mapply(adist, DATA$Nombre_total_1, DATA$Nombre_total_2)


