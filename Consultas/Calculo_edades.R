library(lubridate)
options(scipen=999) ### Esto es para evitar la Notación Cientifica

age = function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}