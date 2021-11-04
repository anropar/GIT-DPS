#Validación por capitulo

Identificador=c("A02","A02_1","A03","A03_1","A01","IdIntegrante")
# Identificador=c("A02","A02_1","A03","A03_1","A01","IdIntegrante","ID_UG","ID_PER")
# Identificador=c("id_ug","id_per","A02_1","A03_1","A01","IdIntegrante")#Sisben IV

############
#Capitulo A#
############

Error_FL_A05 = DATA_HOG[(DATA_HOG$A04 %in% "1" & DATA_HOG$A05 %in% 99)|
                        (DATA_HOG$A04 %in% "2" & !DATA_HOG$A05 %in% 99), c(Identificador,"A04","A05")]

Error_FL_A06 = DATA_HOG[(DATA_HOG$A04 %in% "2") & (DATA_HOG$A06 %in% 99),c(Identificador,"A04","A06")]

Error_FL_A07 = DATA_HOG[DATA_HOG$A04 %in% 1 & !str_detect(DATA_HOG$A07, "[^0-9]"),c(Identificador,"A04","A07")]

Error_FL_A08_1 = DATA_HOG[(DATA_HOG$A04 %in% 1 & DATA_HOG$A08_1 %in% 99)|
                         (!DATA_HOG$A04 %in% 1 & !DATA_HOG$A08_1 %in% 99) ,c(Identificador,"A04","A08_1")]

Error_FL_A08_2 = DATA_HOG[(DATA_HOG$A04 %in% c("2","3") & DATA_HOG$A08_2 %in% 99)|
                           (!DATA_HOG$A04 %in% c("2","3") & !DATA_HOG$A08_2 %in% 99),c(Identificador,"A04","A08_2")]

Error_FL_A08_3 = DATA_HOG[(DATA_HOG$A04 %in% c("2","3") & DATA_HOG$A08_3 %in% 99)|
                           (!DATA_HOG$A04 %in% c("2","3") & !DATA_HOG$A08_3 %in% 99),c(Identificador,"A04","A08_3")]

Error_FL_A09 = DATA_HOG[!between(nchar(DATA_HOG$A09),7,10),c(Identificador,"A09")]

Error_FL_A10 = DATA_HOG[nchar(DATA_HOG$A10)!=10 |
                        !grepl("^3", DATA_HOG$A10), c(Identificador,"A10")]

Error_FL_A11 = DATA_HOG[!grepl("@[gmail|hotmail|yahoo|outlook]",tolower(DATA_HOG$A11)), c(Identificador,"A11")]

id=intersect(intersect(Error_FL_A09$A01, Error_FL_A10$A01), Error_FL_A11$A01)

Error_FL_A10_A11 = DATA_HOG[(is.na(DATA_HOG$A09) & is.na(DATA_HOG$A10) & is.na(DATA_HOG$A11))|
                             (DATA_HOG$A01 %in% id),c(Identificador,"A09","A10","A11")]

Error_FL_A09=Error_FL_A09[!is.na(Error_FL_A09$A09),]
Error_FL_A10=Error_FL_A10[!is.na(Error_FL_A10$A10),]
Error_FL_A11=Error_FL_A11[!is.na(Error_FL_A11$A11),]

Error_FL_Latitud = DATA_HOG[!between(as.numeric(as.character(DATA_HOG$Latitud)),-4.226922,13.394123), c(Identificador,"Latitud","Longitud","A03")]
Error_FL_Longitud = DATA_HOG[!between(as.numeric(as.character(DATA_HOG$Longitud)),-81.370210,-66.845070), c(Identificador,"Latitud","Longitud","A03")]
Error_FL_Altitud = DATA_HOG[is.na(DATA_HOG$Altitud)|
                             !between(as.numeric(DATA_HOG$Altitud),0,6000),c(Identificador,"Altitud")]

############
#Capitulo B#
############

Error_FL_B02=DATA_HOG[  (DATA_HOG$B01 %in% c("1","2","3") & DATA_HOG$B02 %in% "0")|
                       (DATA_HOG$B01 %in% c("3","5") & DATA_HOG$B02 %in% "7")|
                       (DATA_HOG$B01 %in% "4" & DATA_HOG$B02 %in% c("1","2","3")),c(Identificador,"B01","B02")]

Error_FL_B03=DATA_HOG[  (DATA_HOG$B01 %in% c("1","2","4") & DATA_HOG$B02 %in% "7" & DATA_HOG$B03 %in% c("1","2"))|
                       (DATA_HOG$B01 %in% 4 & DATA_HOG$B03 %in% c("1","2"))|
                       (DATA_HOG$B02 %in% c("2","3","5") & DATA_HOG$B03 %in% "1")|
                       (DATA_HOG$B02 %in% c("6","7","0") & DATA_HOG$B03 %in% c("1","2")), c(Identificador,"B01","B02","B03")]

Error_FL_B06_1A=DATA_HOG[(((DATA_HOG$B06_1 %in% "1")) & !(DATA_HOG$B06_1A %in% c("0","1","2","3","4","5","6","9"))) |
                          (((DATA_HOG$B06_1 %in% "2")) &  (DATA_HOG$B06_1A %in% c("0","1","2","3","4","5","6","9"))), c(Identificador,"B06_1","B06_1A")]

Error_FL_B06_2 = DATA_HOG[(DATA_HOG$B01 %in% 4 & DATA_HOG$B06_2 %in% 1)|
                           (DATA_HOG$B01 %in% c("1","2") & DATA_HOG$B02 %in% 7 & DATA_HOG$B06_2 %in% 1), c(Identificador,"B06_2")]
Error_FL_B06_3 = DATA_HOG[(DATA_HOG$B01 %in% 4 & DATA_HOG$B06_3 %in% 1)|
                           (DATA_HOG$B01 %in% c("1","2") & DATA_HOG$B02 %in% 7 & DATA_HOG$B06_3 %in% 1), c(Identificador,"B06_3")]

Error_FL_B06_5A=DATA_HOG[(((DATA_HOG$B06_5 %in% "1")) & !(DATA_HOG$B06_5A %in% c("0","1","2","3","4","5","6","9"))) |
                          (((DATA_HOG$B06_5 %in% "2")) &  (DATA_HOG$B06_5A %in% c("0","1","2","3","4","5","6","9"))), c(Identificador,"B06_5","B06_5A")]


for (i in 1:6) {
  x = paste0("B08_",i)
  y = paste0("B08_",i,"A")
  assign(paste0("Error_FL_B08_",i,"A"), DATA_HOG[(DATA_HOG[[x]] %in% 1 & !between(DATA_HOG[[y]],1,10)) |
                                                  (!DATA_HOG[[x]] %in% 1 & between(DATA_HOG[[y]],1,10)), c(Identificador, x, y)])
}

############
#Capitulo C#
############

Error_FL_C01=DATA_HOG[ (DATA_HOG$B01 %in% c("1","2") & DATA_HOG$B02 %in% 7 & !DATA_HOG$C01 %in% c("1","2","3"))|
                      (DATA_HOG$B01 %in% 3 & !DATA_HOG$C01 %in% c("1","2","3"))|
                      (DATA_HOG$B01 %in% 4 & DATA_HOG$C01>1),c(Identificador,"B01","B02","C01")]

Error_FL_C02=DATA_HOG[(DATA_HOG$C02>DATA_HOG$C01),c(Identificador,"C01","C02")]
Error_FL_C03=DATA_HOG[(DATA_HOG$C03>DATA_HOG$C02),c(Identificador,"C02","C03")]
Error_FL_C04=DATA_HOG[(DATA_HOG$C04>DATA_HOG$C03) | DATA_HOG$C04>15,c(Identificador,"C03","C04")]

Error_FL_C05=DATA_HOG[(DATA_HOG$B06_2 %in% 1 & !DATA_HOG$C05 %in% 1)|
                       (DATA_HOG$B06_2 %in% 2 & DATA_HOG$C05 %in% 1)|
                       (DATA_HOG$B01 %in% 4 & DATA_HOG$C05 %in% c("1","2"))|
                       (DATA_HOG$B01 %in% c("1","2") & DATA_HOG$B02 %in% 7 & DATA_HOG$C05 %in% 1) |
                       (DATA_HOG$B01 %in% c("1","2") & DATA_HOG$C05 %in% 5), c(Identificador,"B01","B02","B06_2","C05")]

Error_FL_C06=DATA_HOG[DATA_HOG$C05 %in% 5 & (DATA_HOG$C06 %in% c("1","2")), c(Identificador,"C05","C06")]
Error_FL_C07=DATA_HOG[DATA_HOG$C05 %in% 5 & (DATA_HOG$C07 %in% c("1","2","3")), c(Identificador,"C05","C07")]

Error_FL_C09=DATA_HOG[(DATA_HOG$B06_5  %in% 2 & DATA_HOG$C09 %in% 1),c(Identificador,"B06_5","C09")]

Error_FL_C10  = DATA_HOG[(DATA_HOG$C09 %in% 1 & !DATA_HOG$C10 %in% c(1,2)) |
                         (DATA_HOG$C09 %in% c(2,3,4,5,6,7,8,99) & !DATA_HOG$C10 %in% 99), c(Identificador,"C09","C10")]

Error_FL_C10_1 = DATA_HOG[(DATA_HOG$C10 %in% 2 & !between(DATA_HOG$C10_1,1,6)) |
                           (!DATA_HOG$C10 %in% 2 & between(DATA_HOG$C10_1,1,6)), c(Identificador,"C09","C10","C10_1")]

Error_FL_C11  = DATA_HOG[(DATA_HOG$C09 %in% 1 & !DATA_HOG$C11 %in% c(1,2))|
                        (!DATA_HOG$C09 %in% 1 & !DATA_HOG$C11 %in% 99), c(Identificador,"C09","C10","C10_1","C11","C11_1")]

Error_FL_C11_1 = DATA_HOG[(DATA_HOG$C11 %in% 2 & !between(DATA_HOG$C11_1,1,23)) |
                           (!DATA_HOG$C11  %in% 2 & between(DATA_HOG$C11_1,1,23)), c(Identificador,"C09","C10","C10_1","C11","C11_1")]

Error_FL_C13=DATA_HOG[(DATA_HOG$B06_4 %in% 2 & DATA_HOG$C13 %in% "1"), c(Identificador,"B06_4","C13")]

Error_FL_C14=DATA_HOG[(DATA_HOG$B01 %in% 3 & !DATA_HOG$C05 %in% 5 & !DATA_HOG$C14 %in% 2)|
                       (DATA_HOG$B01 %in% c("1","2") & !DATA_HOG$C14 %in% 1),c(Identificador,"B01","C05","C14")]

Error_FL_C15=DATA_HOG[(DATA_HOG$C14 %in% 1 & !DATA_HOG$C15 %in% c("1","2","3","4","5"))|
                       ((DATA_HOG$C03==DATA_HOG$C04 & DATA_HOG$C04!=0) & DATA_HOG$C15 %in% 2) |
                       (DATA_HOG$C14 %in% 2 & DATA_HOG$C15 %in% 1), c(Identificador,"C03","C04","C14","C15")]

Error_FL_C16=DATA_HOG[(DATA_HOG$B01 %in% c("1","2") & DATA_HOG$B02 %in% 7 & DATA_HOG$C16 %in% 2)|
                       (DATA_HOG$B01 %in% 4 & DATA_HOG$C16 %in% 2),c(Identificador,"B01","B02","C16")]

Error_FL_C18 = DATA_HOG[(DATA_HOG$C14 %in% 2 & DATA_HOG$C15 %in% 5 & !DATA_HOG$C18 %in% 99)|
                       (DATA_HOG$B01 %in% 4 & DATA_HOG$C18 %in% 2)|
                       (DATA_HOG$B06_1 %in% 2 & DATA_HOG$C18 %in% 1)|
                       (DATA_HOG$B06_3 %in% 2 & DATA_HOG$C18 %in% 2), c(Identificador,"B01","B06_1","B06_3","C18")]

############
#Capitulo D#
############

############
#Capitulo E#
############

Error_FL_E01_1=DATA[is.na(DATA$E01_1) | DATA$E01_1 %in% "" | !between(nchar(as.character(DATA$E01_1)),2,42) | grepl("[[:digit:]]", DATA$E01_1), c(Identificador,"E01_1")]
Error_FL_E01_2=DATA[nchar(as.character(DATA$E01_2))<=1 | grepl("[[:digit:]]", DATA$E01_1), c(Identificador,"E01_2")]
Error_FL_E01_3=DATA[is.na(DATA$E01_3) | DATA$E01_3 %in% "" | !between(nchar(as.character(DATA$E01_3)),2,47) | grepl("[[:digit:]]", DATA$E01_1), c(Identificador,"E01_3")]
Error_FL_E01_4=DATA[nchar(as.character(DATA$E01_4))<=1 | grepl("[[:digit:]]", DATA$E01_1), c(Identificador,"E01_4")]

Error_FL_E02_1= DATA[!between(DATA$E02_1,0,120) | DATA$E02_1!=DATA$E02_1, c(Identificador,"E02_1","E02_1")]

Error_FL_E06_1=DATA[(DATA$E06 %in% 2 & DATA$E06_1 %in% 999), c(Identificador,"E06","E06_1")]

###

Error_FL_JEFE_DUPLICADO = DATA[DATA$E14 %in% 1,] %>% group_by(A01) %>% mutate(JEFES=n()) #Se construye dataframe con el conteo de jefes de hogar.
Error_FL_JEFE_DUPLICADO = as.data.frame(Error_FL_JEFE_DUPLICADO[Error_FL_JEFE_DUPLICADO$JEFES!=1, c(Identificador,"E14","E08","E09")])#Si un hogar tiene más de un jefe de hogar o no tiene se asigna al error.
Error_FL_SIN_JEFE = DATA[c(Identificador,"E08","E09","E14")] %>% group_by(A01) %>% filter(all(E14!="1"))
Error_FL_SIN_JEFE = as.data.frame(Error_FL_SIN_JEFE[!duplicated(Error_FL_SIN_JEFE$A01),])
Error_FL_CONYUGUE_DUPLICADO = DATA[DATA$E14 %in% 2,] %>% group_by(A01) %>% mutate(CONYUGUES=n()) #Se construye dataframe con el conteo de jefes de hogar.
Error_FL_CONYUGUE_DUPLICADO = as.data.frame(Error_FL_CONYUGUE_DUPLICADO[Error_FL_CONYUGUE_DUPLICADO$CONYUGUES!=1, c(Identificador,"E14","E08","E09")])#Si un hogar tiene más de un jefe de hogar o no tiene se asigna al error.

DATA = DATA[!DATA$A01 %in% Error_FL_SIN_JEFE$A01,] %>%
  group_by(A01) %>%
  mutate(Edad_Jefe = E02_1[E14==1])

#Revisar validación
Error_FL_E14 = DATA[(DATA$E02_1<14 & DATA$E14 %in% c("1","2","5","8","9"))|
                   (DATA$E02_1<DATA$Edad_Jefe & DATA$E14 %in% c("5","8"))|
                   (DATA$E02_1>DATA$Edad_Jefe & DATA$E14 %in% 4),c(Identificador,"E14","Edad_Jefe","E02_1")]

Error_FL_E15 = DATA[(DATA$E14 %in% 2 & DATA$E15 %in% c("3","4","5"))|
                   (DATA$E02_1<14 & !DATA$E15 %in% "5"), c(Identificador,"E02_1","E14","E15")]

Error_FL_E16 = DATA[(DATA$E15 %in% c("3","4","5") & !DATA$E16 %in% 99)|
                   (!DATA$E15 %in% c("3","4","5") & DATA$E16 %in% 99)|
                   (DATA$Total_Personas==1 & !DATA$E16 %in% 99),c(Identificador,"Total_Personas","E15","E16")]

############
#Capitulo F#
############
DATA$Discapacidad=rowSums(DATA[grep("F01", names(DATA), value = TRUE)]=="1")

Error_FL_F01=DATA[DATA$Discapacidad>7 | (DATA$Discapacidad>1 & DATA$F01_8 %in% 1),c(Identificador,grep("F01", names(DATA), value = TRUE),"Discapacidad")]

Reglas_F01 = DATA$F03 %in% 2 & DATA$E03 %in% 2 & between(DATA$E02_1,8,59)
Reglas_F02 = DATA$F03 %in% 2 & DATA$E03 %in% 2 & DATA$E02_1>=60
Reglas_F03 = DATA$F03 %in% 2 & DATA$E02_1<8
Reglas_F04 = DATA$F03 %in% 2 & DATA$E03 %in% 1 & DATA$E14 %in% 1
Reglas_F05 = DATA$F03 %in% 2 & DATA$E03 %in% 1 & !DATA$E14 %in% 1 & DATA$E02_1>=12
Reglas_F06 = DATA$F03 %in% 2 & DATA$E03 %in% 1 & DATA$E02_1<12

Error_FL_F04=DATA[(DATA$F03 %in% 1 & DATA$F04 %in% 99)|
                 (!DATA$F03 %in% 1 & !DATA$F04 %in% 99)|
                  (Reglas_F01 & !DATA$F04 %in% 99)|
                  (Reglas_F02 & !DATA$F04 %in% 99)|
                  (Reglas_F03 & !DATA$F04 %in% 99)|
                  (Reglas_F04 & !DATA$F04 %in% 99)|
                  (Reglas_F05 & !DATA$F04 %in% 99)|
                  (Reglas_F06 & !DATA$F04 %in% 99), c(Identificador,"E02_1","E03","F03","F04")]

Error_FL_F05=DATA[(DATA$F04 %in% 1 & DATA$F05 %in% 99)|
                (!DATA$F04 %in% 1 & !DATA$F05 %in% 99)|
                  (Reglas_F01 & !DATA$F04 %in% 99)|
                  (Reglas_F02 & !DATA$F04 %in% 99)|
                  (Reglas_F03 & !DATA$F04 %in% 99)|
                  (Reglas_F04 & !DATA$F04 %in% 99)|
                  (Reglas_F05 & !DATA$F04 %in% 99)|
                  (Reglas_F06 & !DATA$F04 %in% 99), c(Identificador,"E02_1","E03","E14","F04","F05")]

Error_FL_F06=DATA[(DATA$E03 %in% 1 & !DATA$F06 %in% 99) &
                  ((DATA$E03 %in% 2 & between(DATA$E02_1,8,59) & DATA$F06 %in% 99)|
                 (!DATA$E03 %in% 2 & !DATA$F06 %in% 99)|
                   (Reglas_F02 & !DATA$F04 %in% 99)|
                   (Reglas_F03 & !DATA$F04 %in% 99)|
                   (Reglas_F04 & !DATA$F04 %in% 99)|
                   (Reglas_F05 & !DATA$F04 %in% 99)|
                   (Reglas_F06 & !DATA$F04 %in% 99)), c(Identificador,"E03","E02_1","E14","F03","F06")]

Error_FL_F07=DATA[(DATA$E03 %in% 1 & !DATA$F06 %in% 99) &
                   ((DATA$E03 %in% 2 & DATA$E02_1>=8 & DATA$F07 %in% 99)|
                   (DATA$E03 %in% 2 & !DATA$E02_1>=8 & !DATA$F07 %in% 99)|
                   (!DATA$E03 %in% 2 & !DATA$F07 %in% 99)|
                   (Reglas_F03 & !DATA$F04 %in% 99)|
                   (Reglas_F04 & !DATA$F04 %in% 99)|
                   (Reglas_F05 & !DATA$F04 %in% 99)|
                   (Reglas_F06 & !DATA$F04 %in% 99)), c(Identificador,"F03","E03","E02_1","F07")]

rm(list = ls()[ls() %in% grep("^Reglas_F",ls(),value = TRUE)])
############
#Capitulo G#
############

Error_FL_G01 = DATA[(DATA$E02_1<5 & !DATA$G01 %in% c("1","2","3","4","5","6","7","8"))|
                  (!DATA$E02_1<5 & DATA$G01 %in% c("1","2","3","4","5","6","7","8")), c(Identificador,"E02_1","G01")]

Error_FL_G02 = DATA[(DATA$E02_1<5 & !DATA$G02 %in% c("1","2"))|
                  (!DATA$E02_1<5 & DATA$G02 %in% c("1","2")),c(Identificador,"E02_1","G02")]


############
#Capitulo H#
############
Error_FL_H01 = DATA[(DATA$E02_1>=5 & !DATA$H01 %in% c("1","2"))|
                   (!DATA$E02_1>=5 & DATA$H01 %in% c("1","2")),c(Identificador,"H01","E02_1")]

Error_FL_H02 = DATA[(DATA$E02_1>=5 & !DATA$H02 %in% c("0","1","2","3","4","5","6","7"))|
                    (!DATA$E02_1>=5 & DATA$H02 %in% c("0","1","2","3","4","5","6","7"))|
                     (DATA$H01 %in% 2 & DATA$H02 %in% c("3","4","5","6","7"))|
                    (between(DATA$E02_1,5,8) & DATA$H02 %in% c("3","4","5","6","7"))|
                    (between(DATA$E02_1,9,12) & DATA$H02 %in% c("4","5","6","7")),c(Identificador,"H01","H02","E02_1")]

Error_FL_H02_A = DATA[(DATA$E02_1>=5 & !DATA$H02_A %in% 0:13)|
                       (!DATA$E02_1>=5 & DATA$H02_A %in% 0:13)|
                       (DATA$H02 %in% 99 & !DATA$H02_A %in% 99)|
                        (DATA$H02 %in% 1 & !DATA$H02_A %in% 0:3)|
                        (DATA$H02 %in% 2 & !DATA$H02_A %in% 0:5)|
                        (DATA$H02 %in% 3 & !DATA$H02_A %in% c(0,6:9))|
                        (DATA$H02 %in% 4 & !DATA$H02_A %in% c(0,10:13))|
                        (DATA$H02 %in% 5 & !DATA$H02_A %in% 0:4)|
                        (DATA$H02 %in% 6 & !DATA$H02_A %in% 0:6)|
                        (DATA$H02 %in% 7 & !DATA$H02_A %in% 0:4), c(Identificador,"H02_A","H02","E02_1")]#Debería ser mayor o igual a 5

Error_FL_H03 = DATA[(DATA$E02_1>=5 & DATA$H03 %in% 99)|
                    (!DATA$E02_1>=5 & !DATA$H03 %in% 99), c(Identificador,"E02_1","H03")]

############
#Capitulo I#
############

Error_FL_I01=DATA[(DATA$E02_1>=8 & !DATA$I01 %in% c("1","2","3","4","5","6","7","0"))|
                   (DATA$E02_1<8 & !DATA$I01 %in% 99)|
                   (DATA$E02_1<12 & DATA$I01 %in% 5)|
                   (DATA$E02_1<18 & DATA$I01 %in% 6)|
                   (!DATA$E02_1<8 & DATA$E14 %in% 15 & !DATA$I01 %in% 1), c(Identificador,"E14","E02_1","I01")]

Error_FL_I03=DATA[(!DATA$E02_1>=8 & !DATA$I03 %in% 999)|
                      (DATA$E02_1>=8 & DATA$I01 %in% 2 & !between(DATA$I03,0,260))|
                      (!DATA$I01 %in% 2 & !DATA$I03 %in% 999)|
                      (DATA$E02_1>=8 & DATA$I01 %in% c("0","1","3","5","6","7") & !DATA$I03 %in% 999), c(Identificador,"E02_1","I01","I03")]

############
#Capitulo J#
############
Error_FL_J01= DATA[(!DATA$E02_1>=8 & DATA$J01 %in% c("1","2","3","4","5","6","7","8","9","10"))|
                    (DATA$E02_1<8 & !DATA$J01 %in% 99)|
                    (DATA$H01 %in% 2 & DATA$J01 %in% 4)|
                    (!DATA$H02 %in% c("5","6","7") & DATA$J01 %in% 4)|
                     (DATA$J14 %in% 2 & DATA$J01 %in% 2), c(Identificador,"E02_1","H01","H02","I01","J14","J01")]

#Asalariados#
#############

Error_FL_J03= DATA[(DATA$I01 %in% c("3","5","6","7") & !DATA$J03 %in% 99)|
                    (DATA$J01 %in% c("4","5","6","7") & !DATA$J03 %in% 99)|
                    (DATA$I01 %in% 1 & DATA$J01 %in% c("1","2","3","8") & !between(DATA$J03,0,30000000)), c(Identificador,"E02_1","J01","J03")]


#Independientes#
################
Error_FL_J12=DATA[(DATA$E02_1>=8 & DATA$J01 %in% c("4","5","6","7") & DATA$A04 %in% 1 & (!between(DATA$J12,100,30000000) & !DATA$J12 %in% c(0,9)))|
                    (DATA$E02_1<8 & !DATA$J12 %in% 99)|
                    (DATA$J01 %in% c("1","2","3","8","9","10") & !DATA$J12 %in% 99)|
                    (DATA$E02_1>=8 & DATA$J01 %in% c("4","5","6","7") & DATA$A04 %in% c("2","3") & !DATA$J12 %in% 99)|
                    (DATA$E02_1>=8 & DATA$J01 %in% c("4","5","6","7") & !DATA$A04 %in% c("2","3") & DATA$J12 %in% 99), c(Identificador,"E02_1","A04","J01","J12")]

Error_FL_J13=DATA[(DATA$J01 %in% c("4","5","6","7") & DATA$A04 %in% c("2","3") & DATA$J13 %in% 99)|
                 (DATA$J01 %in% c("4","5","6","7") & DATA$A04 %in% c("2","3") & (!between(DATA$J13,100,30000000) & !DATA$J13 %in% c(0,9)))|
                 (DATA$J01 %in% c("4","5","6","7") & !DATA$A04 %in% c("2","3") & !DATA$J13 %in% 99)|
                  (!DATA$J01 %in% c("4","5","6","7") & DATA$A04 %in% c("2","3") & !DATA$J13 %in% 99)|
                  (!DATA$J01 %in% c("4","5","6","7") & !DATA$A04 %in% c("2","3") & !DATA$J13 %in% 99), c(Identificador,"E02_1","A04","J01","J13")]

Error_FL_J13_1=DATA[(between(DATA$J13,100,30000000) & !between(DATA$J13_1,1,12))|
                      (DATA$J13 %in% c(0,9) & !DATA$J13_1 %in% 0)|
                      (DATA$J13 %in% 99 & !DATA$J13_1 %in% 99), c(Identificador,"J13","J13_1")]

Error_FL_J14=DATA[(DATA$E02_1<14 & !DATA$J14 %in% 99) |
                (DATA$E02_1>=14 & DATA$I01 %in% c("0","1","2","3","4","5","6","7") & DATA$J14 %in% 99), c(Identificador,"E02_1","I01","J14")]

