#Validación por capitulo

Identificador     = c("A01","IdIntegrante","ID OFERTA","DEPARTAMENTO","MUNICIPIO","TIPO DOCUMENTO","NUMERO DOCUMENTO")

setwd(Entradas)
ListadoMunicipios = read_excel("PlantillaRegistrosAdministrativos_20210930.xlsm",  sheet = "ListadoMunicipios")

###########
#Capitulo #
###########
Error_VA_ID_OFERTA                = DATA[grepl("[[:punct:]]", DATA$`ID OFERTA`) | is.na(DATA$`ID OFERTA`) | DATA$`ID OFERTA`=="", c(Identificador,"ID OFERTA")]
Error_VA_DEPARTAMENTO             = DATA[grepl("[[:punct:]]", DATA$DEPARTAMENTO) | !DATA$DEPARTAMENTO %in% ListadoMunicipios$NOMDEPARTAMENTOCONSULTA, c(Identificador,"DEPARTAMENTO")]
Error_VA_CODIGO_DEPARTAMENTO_DANE = DATA[grepl("[[:punct:]]", DATA$`CODIGO DEPARTAMENTO DANE`) | is.na(DATA$`CODIGO DEPARTAMENTO DANE`) | DATA$`CODIGO DEPARTAMENTO DANE`=="", c(Identificador,"CODIGO DEPARTAMENTO DANE")]
Error_VA_MUNICIPIO                = DATA[grepl("[[:punct:]]", DATA$MUNICIPIO) | !DATA$MUNICIPIO %in% ListadoMunicipios$NOMMUNICIPIONACIONAL, c(Identificador,"MUNICIPIO")]
Error_VA_CODIGO_MUNICIPIO_DANE    = DATA[grepl("[[:punct:]]", DATA$`CODIGO MUNICIPIO DANE`) | !DATA$`CODIGO MUNICIPIO DANE` %in% ListadoMunicipios$NOMMUNICIPIONACIONAL, c(Identificador,"CODIGO MUNICIPIO DANE")]
Error_VA_ESTADO_ACCESO_A_OFERTA   = DATA[grepl("[[:punct:]]", DATA$`ESTADO ACCESO A OFERTA`) | !DATA$`ESTADO ACCESO A OFERTA` %in% c("ACCESO EFECTIVO","FERIA DE SERVICIOS","MENSAJE DE TEXTO","REMITIDO"), c(Identificador,"ESTADO ACCESO A OFERTA")]
Error_VA_TIPO_DOCUMENTO           = DATA[grepl("[[:punct:]]", DATA$`TIPO DOCUMENTO`) | !DATA$`TIPO DOCUMENTO` %in% c("Cédula de Ciudadanía","Cédula de Extranjería","Documento Nacional de Identidad (DNI) del país de origen","Permiso especial de permanencia (PEP) para ciudadanos venezolanos","Registro Civil","Tarjeta de Identidad"), c(Identificador, "TIPO DOCUMENTO")]
Error_VA_NUMERO_DOCUMENTO         = DATA[grepl("[[:punct:]]", DATA$`NUMERO DOCUMENTO`) | is.na(DATA$`NUMERO DOCUMENTO`) | DATA$`NUMERO DOCUMENTO` %in% "" | !between(nchar(DATA$`NUMERO DOCUMENTO`),4,12), c(Identificador, "NUMERO DOCUMENTO")]
Error_VA_PRIMER_NOMBRE            = DATA[grepl("[[:punct:]]", DATA$`PRIMER NOMBRE`) | DATA$`PRIMER NOMBRE` %in% "" | !between(nchar(as.character(DATA$`PRIMER NOMBRE`)),0,42), c(Identificador,"PRIMER NOMBRE")]
Error_VA_SEGUNDO_NOMBRE           = DATA[grepl("[[:punct:]]", DATA$`SEGUNDO NOMBRE`) | !between(nchar(as.character(DATA$`SEGUNDO NOMBRE`)),0,34), c(Identificador, "SEGUNDO NOMBRE")]
Error_VA_PRIMER_APELLIDO          = DATA[grepl("[[:punct:]]", DATA$`PRIMER APELLIDO`) | DATA$`PRIMER APELLIDO` %in% "" | !between(nchar(as.character(DATA$`PRIMER APELLIDO`)),0,47), c(Identificador, "PRIMER APELLIDO")]
Error_VA_SEGUNDO_APELLIDO         = DATA[grepl("[[:punct:]]", DATA$`SEGUNDO APELLIDO`) | !between(nchar(as.character(DATA$`SEGUNDO APELLIDO`)),0,44), c(Identificador, "SEGUNDO APELLIDO")]

Error_VA_FECHA_DE_NACIMIENTO        = DATA[is.na(DATA$`FECHA DE NACIMIENTO`) | DATA$`FECHA DE NACIMIENTO` %in% "", c(Identificador,"FECHA DE NACIMIENTO")]
Error_VA_SEXO                       = DATA[!DATA$SEXO  %in% 1:3, c(Identificador,"SEXO")]
Error_VA_FECHA_DE_LA_ATENCION       = DATA[is.na(DATA$`FECHA DE LA ATENCIÓN`) | DATA$`FECHA DE LA ATENCIÓN` %in% "", c(Identificador,"FECHA DE LA ATENCIÓN")]
Error_VA_LOGRO_PRIVACION_GESTIONADA = DATA[!DATA$`LOGRO y/o PRIVACIÓN GESTIONADA` %in% toupper(c("Identificación",
                                                                                                 "Acceso a agua",
                                                                                                 "Actividad productiva",
                                                                                                 "Afiliación a salud",
                                                                                                 "Atención a menores en riesgo de desnutrición",
                                                                                                 "Controles de crecimiento y desarrollo",
                                                                                                 "Derechos sexuales y reproductivos",
                                                                                                 "Educación financiera",
                                                                                                 "Educación inicial",
                                                                                                 "Elementos de apoyo o rehabilitación",
                                                                                                 "Escolarización",
                                                                                                 "Estudios postsecundarios",
                                                                                                 "Ingreso adultos mayores",
                                                                                                 "Ingresos suficientes",
                                                                                                 "Leer y escribir",
                                                                                                 "No hacinamiento",
                                                                                                 "No pisos en tierra",
                                                                                                 "No trabajo infantil",
                                                                                                 "Paredes adecuadas",
                                                                                                 "Registro de personas con discapacidad",
                                                                                                 "Saneamiento básico",
                                                                                                 "Seguridad alimentaria")), c(Identificador,"LOGRO y/o PRIVACIÓN GESTIONADA")]



