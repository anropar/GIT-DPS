
# Correcciones
Cargue_RA_16112021$Marca_Exc = ifelse(paste(Cargue_RA_16112021$`ID OFERTA`, Cargue_RA_16112021$`NUMERO DOCUMENTO`) %in% paste(Consulta_5$`ID OFERTA`, Consulta_5$`NUMERO DOCUMENTO`),1,0)
Consulta_5$Marca_Inc = ifelse(paste(Consulta_5$`ID OFERTA`, Consulta_5$`NUMERO DOCUMENTO`) %in% paste(Cargue_RA_16112021$`ID OFERTA`, Cargue_RA_16112021$`NUMERO DOCUMENTO`),1,0)

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS

write.table(Cargue_RA_16112021, file = paste("Cargue_RA_Exc","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "")
write.table(Consulta_5[c(intersect(names(Cargue_RA_16112021), names(Consulta_5)))], file = paste("Consulta_5_Inc","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "")
write.table(Consulta_5_Inc_22112021[-17], file = paste("Consulta_5_Inc","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "")

# DATA = merge(DATA, Precargue[c("A01","IdIntegrante","E09","E01_1","E01_2","E01_3","E01_4","A03_1")], by.x = c("NUMERO DOCUMENTO","CODIGO MUNICIPIO DANE"), by.y = c("E09","A03_1"), all.x = T)
# Duplicados_DATA = Precargue[duplicated(Precargue$E09) | duplicated(Precargue$E09,fromLast = T),]
# Consulta$Cruce_duplicado = ifelse(paste(Consulta$`NUMERO DOCUMENTO`, Consulta$`CODIGO MUNICIPIO DANE`) %in% paste(Duplicados_Precargue$, Duplicados_Precargue$A03_1),1,0)

write.table(Cargue_RA_16112021, file = paste("Cargue_RA_Exc","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = F)

Consulta_5$Marca_Inc = ifelse(paste(Consulta_5$`NUMERO DOCUMENTO`, Consulta_5$`ESTADO ACCESO A OFERTA`, Consulta_5$`LOGRO y/o PRIVACIÓN GESTIONADA`) %in% 
                              paste(Consulta_5_P1$NUMERO.DOCUMENTO, Consulta_5_P1$ESTADO.ACCESO.A.OFERTA, Consulta_5_P1$LOGRO.y.o.PRIVACIÓN.GESTIONADA),1,0)

diff_total(paste(Oferta$`ID Oferta`, Oferta$`Cód Municipio`), 
           paste(Consulta_5_P1$ID.OFERTA, Consulta_5_P1$CODIGO.MUNICIPIO.DANE))

Consulta_5_P1$Marca_Inc = ifelse(paste(Consulta_5_P1$ID.OFERTA, Consulta_5_P1$CODIGO.MUNICIPIO.DANE) %in% 
                                paste(Oferta$`ID Oferta`, Oferta$`Cód Municipio`),1,0)

setwd(paste(Carpeta,"2. Sabana","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.table(Consulta_5_P1[Consulta_5_P1$Marca_Inc %in% 0,] %>% select(-Marca_Inc), file = paste("Consulta_5_P1","_",format(Sys.time(), "%d%m%Y"),".txt", sep=""), sep = ";", row.names = FALSE, quote = F, na = "")


# Diferencia entre consulta 5 y consulta 5 P1
diff_total(paste(Consulta_5$`LOGRO y/o PRIVACIÓN GESTIONADA`, Consulta$`NUMERO DOCUMENTO`, Consulta_5$`ESTADO ACCESO A OFERTA`), 
           paste(Consulta_5_P1$LOGRO.y.o.PRIVACIÓN.GESTIONADA, Consulta_5_P1$NUMERO.DOCUMENTO, Consulta_5_P1$ESTADO.ACCESO.A.OFERTA))

