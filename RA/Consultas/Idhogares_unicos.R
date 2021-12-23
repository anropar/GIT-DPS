



setwd(paste(Carpeta,"2. Sabana", "Salidas", sep = "/"))

Consulta_5_E1_02122021 = read_delim(paste("E1","Consulta_5_E1_02122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
Consulta_6_E2_02122021 = read_delim(paste("E2","Consulta_6_E2_16122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
Consulta_6_E3_23122021 = read_delim(paste("E3","Consulta_6_E3_23122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

Interseccion = intersect(names(Consulta_5_E1_02122021), intersect(names(Consulta_6_E2_02122021), names(Consulta_6_E3_23122021)))

Consulta_6_AC = rbind(Consulta_5_E1_02122021[Interseccion], Consulta_6_E2_02122021[Interseccion], Consulta_6_E3_23122021[Interseccion])

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

write.table(Consulta_6_AC[!duplicated(Consulta_6_AC$A01),"A01"], file = paste("Consulta_6_AC","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")

nrow(Consulta_5_E1_02122021[!duplicated(Consulta_5_E1_02122021$A01),])
nrow(Consulta_6_E2_02122021[!duplicated(Consulta_6_E2_02122021$A01),])
nrow(Consulta_6_E3_23122021[!duplicated(Consulta_6_E3_23122021$A01),])
nrow(Consulta_6_AC[!duplicated(Consulta_6_AC$A01),])
