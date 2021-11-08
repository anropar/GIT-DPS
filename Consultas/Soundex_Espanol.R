#################
#spanish_soundex#
#################

Soundex_spanish = function(text=NULL, maxCodeLen=NULL) {
  
  print("No olvide instalar previamente las librerias stringi y stringr")
  
  library(stringi)
  library(stringr)
  
  if (!is.null(maxCodeLen)){
  
  #Paso 1.
  text=gsub("[[:space:]]", "", text)
  text =toupper(text)
  text =gsub("[^[:alpha:]]*$", "",text )
  text =gsub("`|\\'", "", iconv(text , to="ASCII//TRANSLIT"))
  text =str_replace_all(text,"[[:punct:]]", "")
  
  #Paso 2.
  text=gsub("A|E|I|O|U|H|W", "",text)
  
  #Paso 3.
  text=str_replace_all(text, c("P" = "0","B|V" = "1", "F|H"="2", "T|D"="3","S|Z|C|X"="4","Y|L|LL"="5","N|Ñ|M"="6","Q|K"="7","G|J"="8","R|RR"="9"))
  
  #Paso 4.
  text=substr(text, 0, maxCodeLen)
  text=str_pad(text, maxCodeLen, "right", pad = "0")
  
  return(text)
  
  }else{
    print('No hay información completa en los argumentos')
  }
   
}
