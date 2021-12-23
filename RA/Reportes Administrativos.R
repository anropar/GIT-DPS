##############################
# 1. Reporte Administrativos #
##############################
setwd(paste(Carpeta,"2. Sabana", "Salidas", sep = "/"))

RA_E1_02122021 = read_delim(paste("E1","RA_E1_02122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
RA_E2_02122021 = read_delim(paste("E2","RA_E2_16122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
RA_E3_23122021 = read_delim(paste("E3","RA_E3_23122021.txt", sep = "/"), ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

Interseccion = intersect(names(RA_E1_02122021), intersect(names(RA_E2_02122021), names(RA_E3_23122021)))

Original_AC = rbind(RA_E1_02122021[Interseccion], RA_E2_02122021[Interseccion], RA_E3_23122021[Interseccion])

setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path),"1. Entradas","Reporte_Administrativos", sep = slash))
ReporteAdministrativos = read_delim("ReporteAdministrativo_22_diciembre.txt", ";", escape_double = FALSE, trim_ws = TRUE)

Original_AC = merge(Original_AC, ReporteAdministrativos[c("ID Oferta","Id_Persona","ID Registro Administrativo")], by.x = c("ID OFERTA","IdIntegrante"), by.y = c("ID Oferta","Id_Persona"), all.x = T)
setnames(Original_AC, old = c("ID OFERTA","ID Registro Administrativo"), new = c("ID_OFERTA","ID_Registro_Administrativo"))

Original_AC =  Original_AC[!is.na(Original_AC$ID_Registro_Administrativo),] %>% group_by(ID_OFERTA, IdIntegrante) %>%
  mutate(ID_RA = toString(ID_Registro_Administrativo)) %>%
  as.data.frame()

Original_AC = Original_AC[!duplicated(Original_AC$ID_RA),]

Original_AC$ID_RA = sapply(strsplit(Original_AC$ID_RA, ","), function(x) paste(unique(x), collapse = ","))
