# Limpieza de texto

limpiador_texto <- function(text) {
  text=toupper(text)#convierte en mayusculas
  text=gsub("[[:punct:]]"," ", text)#Elimina caracteres especiales
  text=gsub(' +',' ',text)
  text= stri_trans_general(str = text, id = "Latin-ASCII")
  text= gsub('[0-9]+', '', text)
  return(text)
}  