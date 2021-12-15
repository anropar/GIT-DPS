################
# Mujer jefe  #
################

Mujer_jefe <- function(DATA,E03,E14) {
  DATA = DATA %>% mutate(Mujer_jefe = ifelse(E03==2 & E14==1,1,0))
  return(DATA)
}


