# Generación de campos

Original$Cruce = ifelse(Original$`NUMERO DOCUMENTO` %in% Precargue$E09,1,0)

Original$Cruce_TIPO_NUMERO = ifelse(paste(Original$`TIPO DOCUMENTO`, Original$`NUMERO DOCUMENTO`) %in% paste(Precargue$E08, as.character(Precargue$E09)),1,0)

Original$Duplicados = ifelse(duplicated(Original$`NUMERO DOCUMENTO`),1,0)

Original$Duplicados_Logro = ifelse(duplicated(paste(Original$`NUMERO DOCUMENTO`, Original$`LOGRO y/o PRIVACIÓN GESTIONADA`)),1,0)

Original$Cruce_Oferta = ifelse(paste(Original$`ID OFERTA`, Original$`CODIGO MUNICIPIO DANE`) %in% paste(Oferta$`ID Oferta`, Oferta$`Cód Municipio`),1,0)

Original$Cruce_Precargue = ifelse(paste(Original$`ID OFERTA`, Original$`CODIGO MUNICIPIO DANE`) %in% paste(Oferta$`ID Oferta`, Oferta$`Cód Municipio`),1,0)
