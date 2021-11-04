# ===============================
#   VALIDACIONES Y CALCULOS ####
# ===============================
#'''En este código se encuentran los pasos para el cálculo de logros, IPM y LP de la Estrategia Unidos. Adicionalmente, contiene la 
# validación de la sabana de caracterización y la comparación entre los cálculos elaborados por el GIT y el Sistema de Información de 
# la Estrategia.'''

# Estos script tienen archivos de entrada que se deben controlar:

# 2.1. Importacion de datos.R (Path: Validaciones)
# Comparacion Logro.R (Path: Validaciones/3. Calculos)
# LOGRO27.R (Path: Validaciones/3. Calculos/Logros)
# LOGRO28.R (Path: Validaciones/3. Calculos/Logros)

Fecha = gsub("-","", Sys.Date())

Carpeta = dirname(rstudioapi::getSourceEditorContext()$path)#El ultimo slash o backslash no se debe incluir

slash = "/"

Entradas=paste(Carpeta,"1. Entradas", sep = slash)# Defina el escritorio de entrada donde están los archivos requeridos.
Salidas =paste(paste(Carpeta,"Salidas","Validacion_", sep = slash), Fecha, sep = "")# Defina el escritorio de salida donde serán enviado los archivos generados.
General =paste(Carpeta,"General", sep = slash)# Defina el escritorio donde se encuentra el script con el nombre de "General.R"

#     LIBRERIAS   ####
setwd(Carpeta)
source("Librerias.R", encoding = "UTF-8")# Las librerias que se usaran

###########################
# 1. Importación de datos #
###########################
setwd(Carpeta)
source("Importacion de datos.R", encoding = "UTF-8")

###############
# 1.1 MUESTRA #
###############
# Si desea realizar los cálculos y validaciones inicialmente con una muestre habilite las siguientes lineas de código

# porcentaje = 0.001# Porcentaje de la muestra
# 
# setwd(Carpeta)
# source("Muestra.R", encoding = "UTF-8")

##########
#Formatos#
##########
# setwd(paste(Carpeta,"2. Sabana","Version", sep = slash))
# source("Formatos.R")# Contiene las variables definidas en el diccionario de datos correspondiente

###########################
# 2. Etapas de validaciOn #
###########################

# Crea carpeta de salida de resultados de la validación
setwd(paste(paste(Carpeta,"2. Sabana", sep = slash),"Salidas", sep = slash))
dir.create(paste0("Validacion_",Fecha), showWarnings = FALSE)#Crea una nueva carpeta

# ValidaciOn de la primera etapa - Nombres de los campos
setwd(paste(Carpeta,"2. Sabana", sep = slash))
source("E1.R")

#########################
#Generacion de variables#
#########################

# Las variables generadas se requieren para calculos posteriores.
# setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
# source("Generacion de campos.R")

# # Se cambian los nombres de los campos para poder avanzar en la validacion
# setwd(paste(Carpeta,"2. Sabana", sep = slash))
# source("Simulacion_OTI.R")

# Validacion de la segunda etapa - Valores admisibles
setwd(paste(Carpeta,"2. Sabana", sep = slash))
source("E2.R")

# Validacion de la tercera etapa - Flujos de los datos
setwd(paste(Carpeta,"2. Sabana", sep = slash))
source("E3.R")

################################
# 6. Estadasticas descriptivas #
################################
view(dfSummary(as.data.frame(DATA)))# Estadística descriptiva del cálculo de DATA

#####################
# 7. Exportaciones  #
#####################
# Exportación de los Registros Administrativos
setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.csv(DATA, file =paste("RA_V1","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)
write.csv(DATA %>% select(-c("A01","IdIntegrante")), file =paste("RA_V2","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)
