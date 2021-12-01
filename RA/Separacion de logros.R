# Registros administrativos

Consulta_5_P1 = read_delim("~/GitHub/GIT-DPS/RA/2. Sabana/Salidas/Otros/RA_Jorge_23112021.txt", 
                           ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                           trim_ws = TRUE)

Consulta_6_P2 = read_delim("~/GitHub/GIT-DPS/RA/2. Sabana/Salidas/Otros/Segunda_Entrega_Cargue_25112021.txt", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

Consulta_6_P2 = merge(Consulta_6_P2, Precargue[c("A01","IdIntegrante","E08","E09","E01_1","E01_2","E01_3","E01_4","A03_1")], by.x = c("NUMERO DOCUMENTO"), by.y = c("E09"), all.x = T)

Consulta_5_P1_6_P2 = rbind(Consulta_5_P1[intersect(names(Consulta_5_P1), names(Consulta_6_P2))],
                           Consulta_6_P2[intersect(names(Consulta_5_P1), names(Consulta_6_P2))])

max(str_count(Consulta_5_P1_6_P2$`LOGRO y/o PRIVACIÓN GESTIONADA`, ','))

DF = Consulta_5_P1_6_P2 %>% separate(`LOGRO y/o PRIVACIÓN GESTIONADA`, c("Logros 1", "Logros 2", "Logros 3", "Logros 4", "Logros 5", "Logros 6"), ",")

DF_1 = DF %>% select(-paste("Logros", 2:6))
DF_2 = DF %>% select(-paste("Logros", c(1,3:6)))
DF_3 = DF %>% select(-paste("Logros", c(1:2,4:6)))
DF_4 = DF %>% select(-paste("Logros", c(1:3,5:6)))
DF_5 = DF %>% select(-paste("Logros", c(1:4,6)))
DF_6 = DF %>% select(-paste("Logros", c(1:5)))

setnames(DF_1, old = "Logros 1", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(DF_2, old = "Logros 2", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(DF_3, old = "Logros 3", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(DF_4, old = "Logros 4", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(DF_5, old = "Logros 5", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(DF_6, old = "Logros 6", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")

DF_1 = DF_1[!is.na(DF_1$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
DF_2 = DF_2[!is.na(DF_2$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
DF_3 = DF_3[!is.na(DF_3$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
DF_4 = DF_4[!is.na(DF_4$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
DF_5 = DF_5[!is.na(DF_5$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
DF_6 = DF_6[!is.na(DF_6$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]

DF = rbind(DF_1, DF_2, DF_3, DF_4, DF_5, DF_6)

setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path),"1. Entradas","Reporte_Administrativos", sep = slash))
ReporteAdministrativos <- read_delim("ReporteAdministrativos_28Nov.txt", ";", escape_double = FALSE, trim_ws = TRUE)

max(str_count(ReporteAdministrativos$`Logro asociado`, ','))

RAC = ReporteAdministrativos %>% separate("Logro asociado", c("Logros 1", "Logros 2", "Logros 3", "Logros 4", "Logros 5", "Logros 6"), ",")

RAC_1 = RAC %>% select(-paste("Logros", 2:6))
RAC_2 = RAC %>% select(-paste("Logros", c(1,3:6)))
RAC_3 = RAC %>% select(-paste("Logros", c(1:2,4:6)))
RAC_4 = RAC %>% select(-paste("Logros", c(1:3,5:6)))
RAC_5 = RAC %>% select(-paste("Logros", c(1:4,6)))
RAC_6 = RAC %>% select(-paste("Logros", c(1:5)))

setnames(RAC_1, old = "Logros 1", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(RAC_2, old = "Logros 2", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(RAC_3, old = "Logros 3", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(RAC_4, old = "Logros 4", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(RAC_5, old = "Logros 5", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")
setnames(RAC_6, old = "Logros 6", new = "LOGRO.y.o.PRIVACIÓN.GESTIONADA")

RAC_1 = RAC_1[!is.na(RAC_1$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
RAC_2 = RAC_2[!is.na(RAC_2$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
RAC_3 = RAC_3[!is.na(RAC_3$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
RAC_4 = RAC_4[!is.na(RAC_4$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
RAC_5 = RAC_5[!is.na(RAC_5$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]
RAC_6 = RAC_6[!is.na(RAC_6$LOGRO.y.o.PRIVACIÓN.GESTIONADA),]


RAC = rbind(RAC_1, RAC_2, RAC_3, RAC_4, RAC_5, RAC_6)

rm(list = ls()[ls() %in% grep("^DF_|^RAC_",ls(),value = TRUE)])



#
diff_total(paste(ReporteAdministrativos$Id_Persona, 
                 toupper(ReporteAdministrativos$`Logro asociado`),
                 toupper(ReporteAdministrativos$`Estado de acceso a la Oferta`)), 
           paste(Consulta_5_P1_6_P2$IdIntegrante,
                 toupper(Consulta_5_P1_6_P2$`LOGRO y/o PRIVACIÓN GESTIONADA`),
                 toupper(Consulta_5_P1_6_P2$`ESTADO ACCESO A OFERTA`)))

diff_total(paste(RAC$Id_Persona, 
                 toupper(RAC$LOGRO.y.o.PRIVACIÓN.GESTIONADA),
                 toupper(RAC$`Estado de acceso a la Oferta`)), 
           paste(DF$IdIntegrante,
                 toupper(DF$LOGRO.y.o.PRIVACIÓN.GESTIONADA),
                 toupper(DF$`ESTADO ACCESO A OFERTA`)))

ReporteAdministrativos$Cruce_P1 = ifelse(paste(ReporteAdministrativos$Id_Persona, 
                                                                      toupper(ReporteAdministrativos$`Logro asociado`),
                                                                      toupper(ReporteAdministrativos$`Estado de acceso a la Oferta`)) %in%
                                                    paste(Consulta_5_P1_6_P2$IdIntegrante,
                                                          toupper(Consulta_5_P1_6_P2$`LOGRO y/o PRIVACIÓN GESTIONADA`),
                                                          toupper(Consulta_5_P1_6_P2$`ESTADO ACCESO A OFERTA`)),1,0)

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
write.table(ReporteAdministrativos, file = paste("ReporteAdministrativos_",Entrega,"_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "", fileEncoding = "UTF-8")

view(dfSummary(as.data.frame(ReporteAdministrativos)))# Estadística descriptiva del cálculo de DATA
