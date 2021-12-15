###########################
# 8. Perfiles de hogares  #
###########################

Perfiles <- function(DATA,A01,E02_1) {

  DATA= DATA %>% group_by(A01) %>% mutate(PERFIL_HOG = ifelse(any(E02_1<6) , "INICIAL",
                                                              ifelse(any(E02_1>=6 & E02_1<=17),"CONSOLIDACION",
                                                                     ifelse(all(E02_1>=18) & !all(E02_1>=65),"AFIANZAMIENTO",
                                                                            ifelse(all(E02_1>=65),"MAYOR","NA")))))

  return(DATA)
}
