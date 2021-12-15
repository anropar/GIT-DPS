


Incidencias_nombre_Apellido_03_Dic <- read_delim("C:/Users/andres.romero/Downloads/RegistrosAdministrativos_Nombre_Apellido_01Dic21.csv",
                                                               "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
                                                               trim_ws = TRUE)


setwd("~/GitHub/GIT-DPS/Consultas")
source("Limpiador de textos.R")

Incidencias_nombre_Apellido_03_Dic$Nombres_RA = paste(Incidencias_nombre_Apellido_03_Dic$`PRIMER NOMBRE`, limpiador_texto(Original$`PRIMER APELLIDO`))
Incidencias_nombre_Apellido_03_Dic$Nombres_PR = paste(Original$E01_1, Original$E01_3)

Incidencias_nombre_Apellido_03_Dic$Dist_Nombres = mapply(adist, limpiador_texto(Incidencias_nombre_Apellido_03_Dic$primerNombre), limpiador_texto(Incidencias_nombre_Apellido_03_Dic$`Nombre Administrtivo`))
Incidencias_nombre_Apellido_03_Dic$Dist_Apellidos = mapply(adist, limpiador_texto(Incidencias_nombre_Apellido_03_Dic$primerApellido), limpiador_texto(Incidencias_nombre_Apellido_03_Dic$`Apellido Administrativo`))


Incidencias_nombre_Apellido_03_Dic$Dist_Nombres_Porc = (Incidencias_nombre_Apellido_03_Dic$Dist_Nombres/nchar(Incidencias_nombre_Apellido_03_Dic$primerNombre))*100
Incidencias_nombre_Apellido_03_Dic$Dist_Nombres_Dummy = ifelse(!Incidencias_nombre_Apellido_03_Dic$Dist_Nombres_Porc>=50,1,0)

Incidencias_nombre_Apellido_03_Dic$Dist_Apellidos_Porc = (Incidencias_nombre_Apellido_03_Dic$Dist_Apellidos/nchar(Incidencias_nombre_Apellido_03_Dic$primerApellido))*100
Incidencias_nombre_Apellido_03_Dic$Dist_Apellidos_Dummy = ifelse(!Incidencias_nombre_Apellido_03_Dic$Dist_Apellidos_Porc>=50,1,0)

# Exportaciones
setwd(paste(Carpeta,"2. Sabana","Salidas","Consultas varias", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

write.table(Incidencias_nombre_Apellido_03_Dic, file = paste("RA_Nombres_No_Cruzan_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "ISO-8859-1")
write.csv(Incidencias_nombre_Apellido_03_Dic, file = paste("RA_Nombres_No_Cruzan_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

