# Generación de campos

# DATA$EdadActual = as.integer(age_calc(as.Date(DATA$E02, format = "%d/%m/%Y"), enddate = as.Date(DATA$FechaInicio, format = "%d/%m/%Y"), units = "years", precise = TRUE))

DATA = DATA %>% group_by(A01) %>% mutate(Total_Personas = n())
