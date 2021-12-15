###############
#Fonetico 2000#
###############

fonetico=function(text=NULL){

  print("No olvide instalar previamente las librerias stringi, stringr y phonics")

  library(stringi)
  library(stringr)
  library(phonics)

  if (!is.null(text)){
    text=gsub("¥|Ð","Y|D",text)
    text = gsub(' +',' ',text)
    text = str_trim(text, "left")
    text = str_trim(text, "right")
    text=str_replace_all(gsub("`|\\'", "", toupper(text)),"[[:punct:]]", "")
    text=str_replace_all(text,"[^[:graph:]]", " ")
    text=stri_trans_general(text,"Latin-ASCII")
    text=soundex(text, maxCodeLen = 20L, clean = FALSE)
    return(text)

  }else{
    print('No hay información')
  }

}
