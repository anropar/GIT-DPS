################
# Ciclo vital  #
################

Ciclo <- function(DATA,E02_1) {

  DATA = DATA %>% mutate(CICLOVITAL = ifelse(E02_1<=5, "Primera Infancia",
                                             ifelse((E02_1>=6 & E02_1<=11),"Niñez",
                                                    ifelse((E02_1>=12 & E02_1<=17),"Adolescencia",
                                                           ifelse((E02_1>=18 & E02_1<=24),"Juventud",
                                                                  ifelse((E02_1>=25 & E02_1<=59),"Adulto",
                                                                         ifelse(E02_1>59,"Adulto mayor",
                                                                                "NA")))))))
  return(DATA)
}
