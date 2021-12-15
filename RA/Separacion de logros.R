# Registros administrativos

setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path),"1. Entradas","Reporte_Administrativos", sep = slash))
ReporteAdministrativos = read_delim("ReporteAdministrativo_09_dic.txt", ";", escape_double = FALSE, trim_ws = TRUE)
ReporteAdministrativos = ReporteAdministrativos[ReporteAdministrativos$Fecha_de_Cargue>="2021-12-01",]

# Genera la base de datos con los logros separados y luego apilados
DF = Consulta_5 %>% mutate(`LOGRO y/o PRIVACIÓN GESTIONADA` = strsplit(as.character(`LOGRO y/o PRIVACIÓN GESTIONADA`), ",")) %>% 
          unnest(`LOGRO y/o PRIVACIÓN GESTIONADA`)

# Genera la base de datos con los logros separados y luego apilados
RAC = ReporteAdministrativos %>% mutate(`Logro asociado` = strsplit(as.character(`Logro asociado`), ",")) %>% 
        unnest(`Logro asociado`)

#
nrow(ReporteAdministrativos[ReporteAdministrativos$Id_Persona %in% "No pertenece",])

diff_total(paste(ReporteAdministrativos$Id_Persona, 
                 toupper(ReporteAdministrativos$`Logro asociado`),
                 toupper(ReporteAdministrativos$`Estado de acceso a la Oferta`)), 
           paste(Consulta_5$IdIntegrante,
                 toupper(Consulta_5$`LOGRO y/o PRIVACIÓN GESTIONADA`),
                 toupper(Consulta_5$`ESTADO ACCESO A OFERTA`)))

diff_total(paste(RAC$Id_Persona, 
                 toupper(RAC$`Logro asociado`),
                 toupper(RAC$`Estado de acceso a la Oferta`)), 
           paste(DF$IdIntegrante,
                 toupper(DF$`LOGRO y/o PRIVACIÓN GESTIONADA`),
                 toupper(DF$`ESTADO ACCESO A OFERTA`)))

ReporteAdministrativos$Cruce_P1 = ifelse(paste(ReporteAdministrativos$Id_Persona, 
                                                                      toupper(ReporteAdministrativos$`Logro asociado`),
                                                                      toupper(ReporteAdministrativos$`Estado de acceso a la Oferta`)) %in%
                                                    paste(Consulta_5$IdIntegrante,
                                                          toupper(Consulta_5$`LOGRO y/o PRIVACIÓN GESTIONADA`),
                                                          toupper(Consulta_5$`ESTADO ACCESO A OFERTA`)),1,0)

RAC$Cruce_P1 = ifelse(paste(RAC$Id_Persona, 
                    toupper(RAC$LOGRO.y.o.PRIVACIÓN.GESTIONADA),
                            toupper(RAC$`Estado de acceso a la Oferta`)) %in%
                       paste(DF$IdIntegrante,
                             toupper(DF$LOGRO.y.o.PRIVACIÓN.GESTIONADA),
                             toupper(DF$`ESTADO ACCESO A OFERTA`)),1,0)


Consulta_5_P1_6_P2 = merge(Consulta_5_P1_6_P2, ReporteAdministrativos, by.x = , by.y = )

# Exportaciones
setwd(paste(Carpeta,"2. Sabana","Salidas",Entrega, sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

# Original con marcas
write.table(ReporteAdministrativos[ReporteAdministrativos$Cruce_P1 %in% 0,], file = paste("ReporteAdministrativos_No_Cruzan_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "UTF-8")

write.table(ReporteAdministrativos %>% arrange("ID Registro Administrativo"), file = paste("ReporteAdministrativos_Dic_01","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "UTF-8")


view(dfSummary(as.data.frame(ReporteAdministrativos)))# Estadística descriptiva del cálculo de DATA
