##########################
#Limpiador de direcciones#
##########################

direccion <- function(text) {
  text=toupper(text)#convierte en mayusculas
  text=gsub("[[:punct:]]"," ", text)#Elimina caracteres especiales
  text=gsub(' +',' ',text)
  text=gsub(pattern = '([a-df-km-qs-z])\\1+', replacement = '\\1', x = text)#lidiamos con letras repetidas excepto l,r y e
  text=gsub(pattern = '([lre])\\1{2,}', replacement = '\\1\\1', x = text)#y ahora con l y r
  text=gsub("N º|Nº|Nª|N°|-|º|°|ª| Y | CN |NUMERO|NRO|DIRECCION|ENTRE|[(|)]|CON|FRENTE A LA |FRENTE| AL |B RESVALON | NOMENCLATURA |BARRIO BONEY | POR |SEMAFORO|ESQUINA|VIA QUE COMUNICA", " ", text)
  text=gsub("CARRERA", "KR",text)
  text=gsub("CALLE", "CL",text)
  text=gsub("[C|K][A|]R[R|]E[R|][A|]|KR[A|R|]|[C|K][A|R|][R|A|]|KDX","CARRERA",text)
  text=gsub("[C|K][A-Z|]LL[A|E|]","CALLE",text)
  text=gsub("([[:digit:]])([[:alpha:]])", "\\1 \\2",text)
  text=gsub("([[:digit:]])([[:digit:]])([[:alpha:]])", "\\1\\2 \\3",text)#separa numeros de letras y letras de numeros
  text=gsub("([[:alpha:]])([[:digit:]])|([[:digit:]])([[:alpha:]])", "\\1 \\2",text)#separa numeros de letras y letras de numeros
  text=gsub("AVENIDA|AVEIDA", "AV",text)
  text=gsub("AVENIDA CALLE", "AC",text)
  text=gsub("AVENIDA CARRERA", "AK",text)
  text=gsub("CARRERA", "KR",text)
  text=gsub("CALLE", "CL",text)
  text=gsub("DIAGONAL|DIAGNAL", "DG",text)
  text=gsub("TRANSVERSAL|TRV|TRANS", "TV",text)
  text=gsub("AUTOPISTA|AUTOPITA|AUTPPISTA", "AU",text)
  text=gsub("KRACAS", "CARACAS",text)
  text=gsub(" SIN MAS DATOS", "",text)
  text[text %in% c("SIN DTO","ND","NA","SDIN DATO","SIN DATON","SIN IMFORMACION","VIA PUBLICA","SD","VIA","SIN","NR","",grep("SABE",text,value = TRUE))] = "SIN INFORMACION"
  text[grep("SIN",text, value = FALSE)] = "SIN INFORMACION"
  text=ifelse(str_detect(text, "[:digit:]") & str_detect(text, "[^0-9]"),text,"SIN INFORMACION")#Si la direcci?n no tiene numeros es igual a cero
  text=str_squish(text)#Elimina espacios
  return(text)
}  




