###########################
#       IMPUTACION        #
#                         #
#   Andres Romero Parra   #
###########################

library("ggplot2", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("tidyverse", lib.loc="~/R/win-library/3.4")
library("viridisLite", lib.loc="~/R/win-library/3.4")
library("tidyr", lib.loc="~/R/win-library/3.4")
library("ggthemes", lib.loc="~/R/win-library/3.4")

rm(list = ls())
setwd("~/Datos/ciclo2/DATOS INICIALES/datos_04062018")

Archivo_tabla_de_hogares_con_error_1_ <- read_excel("Datos/PQRS/Revisión consistencia logros/Sin dato/Archivo tabla de hogares con error (1).xlsx")

b<-setdiff(Archivo_tabla_de_hogares_con_error_1_$idIntegranteHogar, Integrantes.Hogares_Acumulado$ID_INTEGRANTE)
c<-setdiff(Archivo_tabla_de_hogares_con_error_1_$idHogar, Integrantes.Hogares_Acumulado$X.U.FEFF.HOGAR)
d<-intersect(Archivo_tabla_de_hogares_con_error_1_$idHogar, Integrantes.Hogares_Acumulado$X.U.FEFF.HOGAR)

Entrega_Hogares_Acumulado_2    <-Entrega_Hogares_Acumulado[Entrega_Hogares_Acumulado$logro19=="SIN DATO",]#Hogares sin dato en el logro 19
Integrantes.Hogares_Acumulado_2<-Integrantes.Hogares_Acumulado[Integrantes.Hogares_Acumulado$X.U.FEFF.HOGAR %in% Entrega_Hogares_Acumulado_2$X.U.FEFF.HOGAR,]#Integrantes de los hogares sin dato en el logro 19


#Bases sin missing values
campo_prueba <- Archivo_tabla_de_hogares_con_error_1_[!(Archivo_tabla_de_hogares_con_error_1_$idIntegranteHogar %in% b),4]
campo_prueba <- Archivo_tabla_de_hogares_con_error_1_[Archivo_tabla_de_hogares_con_error_1_$campo=="I05",]

Integrantes.Hogares_Acumulado_1$vacio<- as.factor(ifelse(Integrantes.Hogares_Acumulado_1$ID_INTEGRANTE %in% campo_prueba,1,0))

