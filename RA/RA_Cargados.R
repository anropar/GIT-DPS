

# Registros administrativos
setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path),"1. Entradas","Reporte_Administrativos", sep = slash))

ReporteAdministrativos_22noviembre <- read_delim("ReporteAdministrativos_22noviembre.txt", 
                                                 ";", escape_double = FALSE, trim_ws = TRUE)

view(dfSummary(as.data.frame(ReporteAdministrativos_22noviembre)))# Estadística descriptiva del cálculo de DATA
