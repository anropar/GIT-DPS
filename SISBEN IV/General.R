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
# # Si desea realizar los cálculos y validaciones inicialmente con una muestre habilite las siguientes lineas de código
#
# porcentaje = 0.001# Porcentaje de la muestra
#
# setwd(Carpeta)
# source("Muestra.R", encoding = "UTF-8")

##########
#Formatos#
##########
setwd(paste(Carpeta,"2. Sabana","Version", sep = slash))
source("Formatos.R")# Contiene las variables definidas en el diccionario de datos correspondiente

#########################
#Generacion de variables#
#########################

# Las variables generadas se requieren para calculos posteriores.
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Generacion de campos.R")

###########################
# 2. Etapas de validacion #
###########################

# Crea carpeta de salida de resultados de la validación
setwd(paste(paste(Carpeta,"2. Sabana", sep = slash),"Salidas", sep = slash))
dir.create(paste0("Validacion_",Fecha), showWarnings = FALSE)#Crea una nueva carpeta

# ValidaciOn de la primera etapa - Nombres de los campos
setwd(paste(Carpeta,"2. Sabana", sep = slash))
source("E1.R")

# Validacion de la segunda etapa - Valores admisibles
setwd(paste(Carpeta,"2. Sabana", sep = slash))
source("E2.R")

# Validacion de la tercera etapa - Flujos de los datos
setwd(paste(Carpeta,"2. Sabana", sep = slash))
source("E3.R")

##################
#   3. Calculos  #
##################

#     CALCULO DE LOGROS    ####
setwd(paste(Carpeta,"3. Calculos", sep = slash))
source("1.1. Logros.R", encoding = "UTF-8")

#     CALCULO DE IPM       ####
setwd(paste(Carpeta,"3. Calculos", sep = slash))
source("1.2. IPM.R", encoding = "UTF-8")

#      CALCULO DE LP       ####
setwd(paste(Carpeta,"3. Calculos", sep = slash))
source("1.3. LP.R", encoding = "UTF-8")

######################
#  4. Comparaciones  #
######################

#      COMPARACION DE LOGROS      ####
setwd(paste(Carpeta,"3. Calculos", sep = slash))
source("2.1. Comparacion Logros.R", encoding = "UTF-8")

#      COMPARACION DE IPM      ####
setwd(paste(Carpeta,"3. Calculos", sep = slash))
source("2.2. Comparacion IPM.R", encoding = "UTF-8")

#      COMPARACION DE LP      ####
setwd(paste(Carpeta,"3. Calculos", sep = slash))# Los datos de SISBEN IV no permiten cálculo de LP
source("2.3. Comparacion LP.R", encoding = "UTF-8")

###################
#  5. Conexiones  #
###################

# setwd(paste(Carpeta,"3. Calculos", sep = slash))
# source("3.1. Conexiones.R", encoding = "UTF-8")

################################
# 6. Estadasticas descriptivas #
################################
view(dfSummary(DATA))# Estadística descriptiva del cálculo de DATA
view(dfSummary(LOGROS_HOG))# Estadística descriptiva del cálculo de LOGROS HOGAR
view(dfSummary(LOGROS_INT))# Estadística descriptiva del cálculo de LOGROS INTEGRANTES
view(dfSummary(IPM_GIT))# Estadística descriptiva del cálculo de IPM
view(dfSummary(LP_GIT))# Estadística descriptiva del cálculo de LP

#####################
# 7. Exportaciones  #
#####################
# Exportación del cálculo de logros
setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.csv(LOGROS_HOG[!duplicated(LOGROS_HOG$A01),], file =paste("LOGROS_HOGARES","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)
write.csv(LOGROS_INT[!duplicated(LOGROS_INT$IdIntegrante),], file =paste("LOGROS_INTEGRANTES","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Exportación del cálculo de IPM
setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de IPM
write.csv(IPM_GIT, file =paste("IPM_GIT","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

# Exportación del cálculo de LP
setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LP
write.csv(LP_GIT, file = paste("LP_GIT","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

#######################################
# 8. Perfiles de hogares & Ciclo vital #
#######################################
setwd(Carpeta)
source("Perfiles.R", encoding = "UTF-8")
source("Ciclo vital.R", encoding = "UTF-8")
source("Mujer jefe.R", encoding = "UTF-8")

DATA = Perfiles(DATA, A01, E02_1)# Genera la columna de perfil del hogar
DATA = Ciclo(DATA, E02_1)# Genera la columna de ciclo vital
DATA = Mujer_jefe(DATA, E03, E14)# Genera la columna de ciclo vital

###############
# 9. Victimas #
###############
setwd(Carpeta)
source("Victimas.R", encoding = "UTF-8")# Genera las marcas de victimas

###############################
# 10. Frecuencias municipales #
###############################
setwd(Carpeta)
source("Frecuencias municipales.R", encoding = "UTF-8")# Genera el archivo de frecuencias municipales

#########################
# 11. Corte certificado #
#########################
setwd(Carpeta)
source("Corte certificado.R", encoding = "UTF-8")# Se define el corte certificado
