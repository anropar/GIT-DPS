#Validación por capitulo

Identificador=c("A02","A02_1","A03","A03_1","A01","IdIntegrante")
# Identificador=c("A02","A02_1","A03","A03_1","A01","IdIntegrante","ID_UG","ID_PER")
# Identificador=c("A02_1","A03_1","A01","IdIntegrante")#Sisben IV

############
#Capitulo A#
############
Error_VA_A01 = DATA_HOG[is.na(DATA_HOG$A01) | DATA_HOG$A01=="",c(Identificador)]
Error_VA_A02 = DATA_HOG[is.na(DATA_HOG$A02) | DATA_HOG$A02=="",c(Identificador)]
Error_VA_A02_1 = DATA_HOG[is.na(DATA_HOG$A02_1) | DATA_HOG$A02_1=="",c(Identificador)]
Error_VA_A03 = DATA_HOG[is.na(DATA_HOG$A03) | DATA_HOG$A03=="",c(Identificador)]
Error_VA_A03_1 = DATA_HOG[is.na(DATA_HOG$A03_1) | DATA_HOG$A03_1=="",c(Identificador)]

Error_VA_A04 = DATA_HOG[!DATA_HOG$A04 %in% c("1","2","3"),c(Identificador,"A04")]

Error_VA_A05 = DATA_HOG[is.na(DATA_HOG$A05) | DATA_HOG$A05=="", c(Identificador,"A05")]
Error_VA_A06 = DATA_HOG[is.na(DATA_HOG$A06) | DATA_HOG$A06 %in% "Otro",c(Identificador,"A06")]

Error_VA_A07 = DATA_HOG[is.na(DATA_HOG$A07) | DATA_HOG$A07=="",c(Identificador,"A07")]

Error_VA_A08_1 = DATA_HOG[is.na(DATA_HOG$A08_1) | DATA_HOG$A08_1=="" | DATA_HOG$A08_1 %in% "Otro", c(Identificador,"A08_1")]
Error_VA_A08_2 = DATA_HOG[is.na(DATA_HOG$A08_2) | DATA_HOG$A08_2=="" | DATA_HOG$A08_2 %in% "Otro", c(Identificador,"A08_2")]
Error_VA_A08_3 = DATA_HOG[is.na(DATA_HOG$A08_3) | DATA_HOG$A08_3=="" | DATA_HOG$A08_3 %in% "Otro", c(Identificador,"A08_3")]


Error_VA_A09 = DATA_HOG[DATA_HOG$A09 %in% "", c(Identificador,"A09")]
Error_VA_A10 = DATA_HOG[DATA_HOG$A10 %in% "", c(Identificador,"A10")]
Error_VA_A11 = DATA_HOG[DATA_HOG$A11 %in% "", c(Identificador,"A11")]

############
#Capitulo B#
############
Error_VA_B01=DATA_HOG[!DATA_HOG$B01 %in% c("1","2","3","4","5"),c(Identificador,"B01")]
Error_VA_B02=DATA_HOG[!DATA_HOG$B02 %in% c("1","2","3","4","5","6","7","0"),c(Identificador,"B02")]
Error_VA_B03=DATA_HOG[!DATA_HOG$B03 %in% c("1","2","3","4","5","6"), c(Identificador,"B03")]

Error_VA_B06_1 = DATA_HOG[!DATA_HOG$B06_1 %in% c("1","2"), c(Identificador,"B06_1")]
Error_VA_B06_1A=DATA_HOG[!DATA_HOG$B06_1A %in% c("0","1","2","3","4","5","6","99"), c(Identificador,"B06_1","B06_1A")]

Error_VA_B06_2 = DATA_HOG[!DATA_HOG$B06_2 %in% c("1","2"), c(Identificador,"B06_2")]
Error_VA_B06_3 = DATA_HOG[!DATA_HOG$B06_3 %in% c("1","2"), c(Identificador,"B06_3")]
Error_VA_B06_4 = DATA_HOG[!DATA_HOG$B06_4 %in% c("1","2"), c(Identificador,"B06_4")]
Error_VA_B06_5 = DATA_HOG[!DATA_HOG$B06_5 %in% c("1","2"), c(Identificador,"B06_5")]

Error_VA_B06_5A=DATA_HOG[!DATA_HOG$B06_5A %in% c("0","1","2","3","4","5","6","99"), c(Identificador,"B06_5A")]

Error_VA_B07 = DATA_HOG[!DATA_HOG$B07 %in% c("1","2","3","4"),c(Identificador,"B07")]

for (i in 1:6) {
  x = paste0("B08_",i)
  assign(paste0("Error_VA_B08_",i), DATA_HOG[!DATA_HOG[[x]] %in% c("1","2"), c(Identificador,x)])
}

for (i in 1:6) {
  y = paste0("B08_",i,"A")
  assign(paste0("Error_VA_B08_",i,"A"), DATA_HOG[!DATA_HOG[[y]] %in% c(1:10,99), c(Identificador, y)])
}

############
#Capitulo C#
############

Error_VA_C01=DATA_HOG[!between(DATA_HOG$C01,1,15),c(Identificador,"C01")]

Error_VA_C02=DATA_HOG[!between(DATA_HOG$C01,0,15),c(Identificador,"C02")]
Error_VA_C03=DATA_HOG[!between(DATA_HOG$C01,0,15),c(Identificador,"C03")]
Error_VA_C04=DATA_HOG[!between(DATA_HOG$C01,0,15),c(Identificador,"C04")]

Error_VA_C05=DATA_HOG[!DATA_HOG$C05 %in% 1:5, c(Identificador,"C05")]

Error_VA_C06=DATA_HOG[!DATA_HOG$C06 %in% c("1","2","99"), c(Identificador,"C05","C06")]
Error_VA_C07=DATA_HOG[!DATA_HOG$C07 %in% c("1","2","3","99"),c(Identificador,"C07")]

Error_VA_C09 = DATA_HOG[!DATA_HOG$C09 %in% c("1","2","3","4","5","6","7","8","9"), c(Identificador,"C09")]
Error_VA_C10  = DATA_HOG[!DATA_HOG$C10 %in% c("1","2","99"), c(Identificador,"C10")]
Error_VA_C10_1 = DATA_HOG[!between(DATA_HOG$C10_1,1,6) & !DATA_HOG$C10_1 %in% 99, c(Identificador,"C10_1")]

Error_VA_C11  = DATA_HOG[!DATA_HOG$C11 %in% c("1","2","99"), c(Identificador,"C11")]
Error_VA_C11_1 = DATA_HOG[!DATA_HOG$C11_1 %in% c(1:23,99), c(Identificador,"C11_1")]

Error_VA_C12=DATA_HOG[!DATA_HOG$C12 %in% c("1","2","3","4","5","6"), c(Identificador,"C12")]
Error_VA_C13=DATA_HOG[!DATA_HOG$C13 %in% c("1","2","3","4","5","6","7"), c(Identificador,"C13")]
Error_VA_C14=DATA_HOG[!DATA_HOG$C14 %in% c("1","2"), c(Identificador,"C14")]
Error_VA_C15=DATA_HOG[!DATA_HOG$C15 %in% c("1","2","3","4","5") ,c(Identificador,"C15")]
Error_VA_C16=DATA_HOG[!DATA_HOG$C16 %in% c("1","2","3","99"), c(Identificador,"C16")]

Error_VA_C18=DATA_HOG[!DATA_HOG$C18 %in% c(1:7,99), c(Identificador,"C18")]

############
#Capitulo D#
############
Error_VA_D01=DATA_HOG[!DATA_HOG$D01 %in% c("1","2","3","4","5"), c(Identificador,"D01")]

############
#Capitulo E#
############
Error_VA_E01_1=DATA[DATA$E01_1 %in% "" | !between(nchar(as.character(DATA$E01_1)),0,30),c(Identificador,"E01_1")]
Error_VA_E01_2=DATA[DATA$E01_2 %in% "" | !between(nchar(as.character(DATA$E01_2)),0,30),c(Identificador,"E01_2")]
Error_VA_E01_3=DATA[DATA$E01_3 %in% "" | !between(nchar(as.character(DATA$E01_3)),0,30),c(Identificador,"E01_3")]
Error_VA_E01_4=DATA[DATA$E01_4 %in% "" | !between(nchar(as.character(DATA$E01_4)),0,30),c(Identificador,"E01_4")]

Error_VA_E02=DATA[is.na(DATA$E02) | DATA$E02 %in% "",c(Identificador,"E02")]
Error_VA_E02_1=DATA[!between(DATA$E02_1,0,120)|
                    is.na(DATA$E02_1), c(Identificador,"E02","E02_1")]

Error_VA_E03=DATA[!DATA$E03 %in% c("1","2","3"),c(Identificador,"E03")]

Error_VA_E06=DATA[!DATA$E06 %in% c("1","2"), c(Identificador,"E06")]
Error_VA_E06_1=DATA[is.na(DATA$E06_1) | DATA$E06_1 %in% "", c(Identificador,"E06_1")]

Error_VA_E08=DATA[!DATA$E08 %in% c("1","2","3","4","5","6","7","8","99"),c(Identificador,"E08")]
Error_VA_E09=DATA[is.na(DATA$E09) | DATA$E09 %in% "",c(Identificador,"E09")]

#Revisar validación
for (i in 1:2) {

  x = paste0("E11_",i)
  assign(paste0("Error_VA_E11_",i), DATA[is.na(DATA[[x]]) | DATA[[x]] %in% "", c(Identificador,x)])

}

Error_VA_E14=DATA[!DATA$E14 %in% 1:19,c(Identificador,"E14")]
Error_VA_E15=DATA[!DATA$E15 %in% 1:5, c(Identificador,"E15")]
Error_VA_E16=DATA[!DATA$E16 %in% c("1","2","99"), c(Identificador,"E15","E16")]

Error_VA_E17=DATA[!DATA$E17 %in% c("1","2","99"), c(Identificador,"E17")]

############
#Capitulo F#
############
#F01-F08
for (i in 1:8) {

  x = paste0("F01_",i)
  assign(paste0("Error_VA_F01_",i), DATA[!DATA[[x]] %in% 1:2, c(Identificador,x)])

}

Error_VA_F02=DATA[!DATA$F02 %in% c("0","1","2","3","9"),c(Identificador,"F02")]
Error_VA_F03=DATA[!DATA$F03 %in% 1:2,c(Identificador,"F03")]

#F04-F23
for (i in 4:7) {

  x = paste0("F",str_pad(i,2,pad = "0"))
  assign(paste0("Error_VA_","F",str_pad(i,2,pad = "0")), DATA[!DATA[[x]] %in% c(1:2,99), c(Identificador,x)])

}


############
#Capitulo G#
############
Error_VA_G01=DATA[!DATA$G01 %in% c("1","2","3","4","5","6","7","8","99"), c(Identificador,"G01")]

Error_VA_G02 = DATA[!DATA$G02 %in% c("1","2","99"),c(Identificador,"G02")]

############
#Capitulo H#
############
Error_VA_H01=DATA[!DATA$H01 %in% c("1","2","99"), c(Identificador,"H01")]
Error_VA_H02=DATA[!DATA$H02 %in% c(0:7,"99"), c(Identificador,"H02")]
Error_VA_H02_A=DATA[!DATA$H02_A %in% c(0:13,"99"), c(Identificador,"H02_A")]

Error_VA_H03=DATA[!DATA$H03 %in% c("1","2","99"), c(Identificador,"H03")]

############
#Capitulo I#
############
Error_VA_I01=DATA[!DATA$I01 %in% c(0:7,"99"), c(Identificador,"I01")]
Error_VA_I03=DATA[!DATA$I03 %in% c(1:260,"999"), c(Identificador,"I03")]

############
#Capitulo J#
############

Error_VA_J01=DATA[!DATA$J01 %in% c(1:10,"99"), c(Identificador,"J01")]

Error_VA_J03=DATA[!between(DATA$J03,99,30000000) & !DATA$J03 %in% c(0,9), c(Identificador,"J03")]

Error_VA_J12=DATA[!between(DATA$J12,100,30000000) & !DATA$J12 %in% c(0,9,99), c(Identificador,"J12")]

Error_VA_J13=DATA[!between(DATA$J13,100,30000000) & !DATA$J13 %in% c(0,9,99), c(Identificador,"J13")]
Error_VA_J13_1=DATA[!DATA$J13_1 %in% c(1:12,0,99), c(Identificador,"J13_1")]

Error_VA_J14=DATA[!DATA$J14 %in% c(1:3,99), c(Identificador,"J14")]
