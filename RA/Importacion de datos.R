################
#CARGA DE DATOS#
################

setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.

#####################
# ESTRATEGIA UNIDOS #
#####################
tictoc::tic("Total")

DATA = read_excel("Muestra_07102021.xlsx")

lazy_dt(DATA)

tictoc::toc()
