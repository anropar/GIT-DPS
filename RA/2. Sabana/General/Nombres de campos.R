#Nombres de campos

#Esta en DATA pero no se encuentra en el diccionario.
Error_Nombres_variables_1=setdiff(names(DATA), Campos)#Identifica las variables que están en la base de datos pero no en el diccionario.

#Esta en el diccionario pero no se encuentra DATA.
Error_Nombres_variables_2=setdiff(Campos, names(DATA))#Identifica las variables que están en el diccionario pero no están en la base de datos.

rm(list = ls()[ls() %in% grep("^capitulo|^[V|v]ar",ls(),value = TRUE)])
