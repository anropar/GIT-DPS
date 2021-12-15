###############################################
# Victimas de desplazamiento forzado y Unidos #
###############################################
setwd(Entradas)

# VICTIMAS = read.table("VICTIMAS_201908.txt","»", header=TRUE, encoding="ISO8859-1", stringsAsFactors=FALSE)
VICTIMAS = read_delim("~/GitHub/GIT-DPS/RA/1. Entradas/UNIVERSO_VICTIMAS_LB.csv", "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
VICTIMAS = VICTIMAS[VICTIMAS$HECHO=="Desplazamiento forzado", c("TIPODOCUMENTO","DOCUMENTO","PRIMERNOMBRE","SEGUNDONOMBRE","PRIMERAPELLIDO","SEGUNDOAPELLIDO","FECHANACIMIENTO","HECHO")]

###################################
#Emparejamiento de ofera y demanda#
###################################
VICTIMAS$TIPODOCUMENTO = recode_factor(VICTIMAS$TIPODOCUMENTO, `RC` = 1, `NUIP` = 1,
                                                              `TI` = 2, `CC` = 3,
                                                              `CE` = 4, `DNI` = 5,
                                                              `PA` = 6, `SR` = 7,
                                                              `PEP` = 8)

setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Fonetico.R")#Se utiliza para dirigir las salidas a las carpetas definidas.

DATA$FONETICO = paste(fonetico(DATA$E01_1),
                      fonetico(DATA$E01_3),
                      fonetico(DATA$E01_4),
                      as.Date(DATA$E02, format = "%Y-%m-%d"))

VICTIMAS$FONETICO = paste(fonetico(VICTIMAS$PRIMERNOMBRE),
                          fonetico(VICTIMAS$PRIMERAPELLIDO),
                          fonetico(VICTIMAS$SEGUNDOAPELLIDO),
                          as.Date(VICTIMAS$FECHANACIMIENTO, format = "%d/%m/%Y"))

DATA$Tipo_Cruce_VICTIMAS = ifelse(paste(DATA$E08, DATA$E09) %in% paste(VICTIMAS$TIPODOCUMENTO, VICTIMAS$DOCUMENTO), "Documento",
                               ifelse(DATA$FONETICO %in% VICTIMAS$FONETICO, "Fonetico",
                                      ifelse((paste(DATA$E08, DATA$E09) %in% paste(VICTIMAS$TIPODOCUMENTO, VICTIMAS$DOCUMENTO) &
                                               DATA$FONETICO %in% VICTIMAS$FONETICO),"Documento y fonetico","No cruzo")))

DATA$VICTIMA = ifelse(DATA$Tipo_Cruce_VICTIMAS %in% c("Documento","Fonetico"),1,0)

DATA = DATA %>% group_by(A01) %>% mutate(VICTIMA = ifelse(any(VICTIMA==1),1,0))

rm(VICTIMAS)
