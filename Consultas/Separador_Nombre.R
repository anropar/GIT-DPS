

library(tidyr)
library(unglue)
library(gsubfn)

count_space=function(text){
   text=nchar(text)-nchar(gsub("[[:space:]]","",text))
  return(text)
}


# Prueba$NOMBRE = paste(Prueba$NOMBRES, Prueba$APELLIDOS)

Prueba$NOMBRE = gsub("\\s+"," ",Prueba$NOMBRE)
Prueba$Espacios=count_space(Prueba$NOMBRE)

Data_1=Prueba[Prueba$Espacios==1,]
Data_2=Prueba[Prueba$Espacios==2,]
Data_3=Prueba[Prueba$Espacios==3,]
Data_4=Prueba[!Prueba$Espacios %in% 1:3,]

Data_1$Primer_Nombre   = sapply(strsplit(Data_1$NOMBRE, ' '), function(x) x[1])
Data_1$Segundo_Nombre = NA
Data_1$Primer_Apellido   = sapply(strsplit(Data_1$NOMBRE, ' '), function(x) x[2])
Data_1$Segundo_Apellido = NA

Data_2$Primer_Nombre   = sapply(strsplit(Data_2$NOMBRE, ' '), function(x) x[1])
Data_2$Segundo_Nombre = NA
Data_2$Primer_Apellido   = sapply(strsplit(Data_2$NOMBRE, ' '), function(x) x[2])
Data_2$Segundo_Apellido = sapply(strsplit(Data_2$NOMBRE, ' '), function(x) x[3])

Data_3$Primer_Nombre   = sapply(strsplit(Data_3$NOMBRE, ' '), function(x) x[1])
Data_3$Segundo_Nombre = sapply(strsplit(Data_3$NOMBRE, ' '), function(x) x[2])
Data_3$Primer_Apellido   = sapply(strsplit(Data_3$NOMBRE, ' '), function(x) x[3])
Data_3$Segundo_Apellido = sapply(strsplit(Data_3$NOMBRE, ' '), function(x) x[4])

Data_4$Primer_Nombre   = sapply(strsplit(Data_4$NOMBRE, ' '), function(x) x[1])
Data_4$Segundo_Nombre = sapply(strsplit(Data_4$NOMBRE, ' '), function(x) x[2])
Data_4$Primer_Apellido   = sapply(strsplit(Data_4$NOMBRE, ' '), function(x) x[3])

Data_4$Apellido_4=sapply(strsplit(Data_4$NOMBRE, ' '), function(x) x[4])
Data_4$Apellido_5=sapply(strsplit(Data_4$NOMBRE, ' '), function(x) x[5])
Data_4$Apellido_6=sapply(strsplit(Data_4$NOMBRE, ' '), function(x) x[6])

Data_4$Segundo_Apellido = apply(Data_4[c("Apellido_4", "Apellido_5","Apellido_6")], 1, function(i){ paste(na.omit(i), collapse = " ") })

Data_4 = Data_4 %>% select(-c("Apellido_4","Apellido_5","Apellido_6"))

Data = rbind(Data_1,Data_2, Data_3, Data_4)
Data = Data %>% select(-Espacios)
