################################
#Validaciones Estrategia DATA#
################################

DATA_HOG=DATA[!duplicated(DATA$A01),]#Genera base de datos a nivel de hogar

############
#VALIDACION#
############
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
#source("Descriptiva.R")# Estadisticas descriptivas de los datos

############
#Duplicados#
############
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Duplicados.R")

################
# Validaciones #
################
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Validacion_FL.R")

######################
# Resumen de errores #
######################
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Errores_vacios_FL.R")

setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Generacion de salidas_T3.R")# Se utiliza para dirigir las salidas a las carpetas definidas.

rm(list = ls()[!ls() %in% grep("^DATA|Error|Resumen|Version|Salidas|Entradas|Carpeta|General|Fecha|slash",ls(),value = TRUE)])
