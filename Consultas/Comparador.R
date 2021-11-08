#################
#Comparador 2000#
#################

diff_total <- function(vec1=NULL,vec2=NULL,escritorio=NULL)
{
  if (!is.null(vec1) & !is.null(vec2)){
    if(is.null(escritorio)){
      Conteos=c(length(setdiff(vec1,vec2)),length(setdiff(vec2,vec1)),length(intersect(vec2,vec1)))
      Casos=c("Diferencias VEC1-VEC2","Diferencias VEC2-VEC1","Intersección")
      data=data.frame(Casos,Conteos)
      return(data)
    }
    else if(!is.null(escritorio)){
      setwd(escritorio)
      Conteos=c(length(setdiff(vec1,vec2)),length(setdiff(vec2,vec1)),length(intersect(vec2,vec1)))
      Casos=c("Diferencias VEC1-VEC2","Diferencias VEC2-VEC1","Intersección")
      data=data.frame(Casos,Conteos)
      
      Diferencias_A_B=setdiff(vec1,vec2)
      Diferencias_B_A=setdiff(vec2,vec1)
      Interseccion=intersect(vec2,vec1)
      
        if (length(Diferencias_A_B)>0){
          write.csv2(Diferencias_A_B, file = paste("Diferencias_A_B","_",format(Sys.time(), "%d_%m_%Y"),".csv",sep = ''), row.names = FALSE)
          }else{
          print('No hay valores en Diferencias_A_B')
        }
      
        if (length(Diferencias_B_A)>0){
          write.csv2(Diferencias_B_A, file = paste("Diferencias_B_A","_",format(Sys.time(), "%d_%m_%Y"),".csv",sep = ''), row.names = FALSE)
        }else{
          print('No hay valores en Diferencias_B_A')
        }
        
        if (length(Interseccion)>0){
          write.csv2(Interseccion, file = paste("Interseccion","_",format(Sys.time(), "%d_%m_%Y"),".csv",sep = ''), row.names = FALSE)
        }else{
          print('No hay valores en Interseccion')
        }
      print("No olvide revisar que la dirección este correctamente definida")
      return(data)
      
    }
  }else{
    print('Falta definir los vectores')
  }
}
