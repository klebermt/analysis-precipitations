##Analisis de datos dudosos
#importar funciones
source("Valores kn.R")

#Establecemos las variables globales
coeSesgo1 <- 0.4 #Sí la asimetría de la estación es mayor
coeSesgo2 <- -0.4 #Sí la asimetría de la e(stación es menor

##Umbral de valores atipicos
getXH <- function(datos){
  library(e1071)
  #coeficiente de asimetría de las precipitaciones máximas logaritmicas
  coeAsimetria <- skewness(datos$pp_log) 
  
  #Logaritmo medio
  x <- mean(datos$pp_log)
  #Valor K para el tamaño de la muestra N
  kn <- setValueKn(nrow(datos))
  #Desviación standar para los logaritmos
  s <- sd(datos$pp_log)
  
  ##Detectar datos dudosos altos y bajos
  #Detectar datos dudosos altos
  xh = x + (kn * s)
  #precipitación máxima aceptada
  pH1 = 10 ^ xh
  #Detectar datos dudosos bajos
  xh = x - (kn * s)
  #precipitación máxima aceptada
  pH2 = 10 ^ xh
  
  #Comprobar que tipo de prueba determinar
  if(coeAsimetria > coeSesgo1){
    #Detectar datos dudosos altos
    print("Se detectan datos dudosos altos")
    return(c(pH1,pH2))
  } else if (coeAsimetria < coeSesgo2){
    #Detectar datos dudosos bajos
    print("Se detectan datos dudosos bajos")
    return(c(pH1,pH2))
  } else if (coeAsimetria < coeSesgo2 && coeAsimetria > coeSesgo1){
    ##Detectar datos dudosos altos y bajos
    print("Se detectan datos dudosos altos y bajos")
    return(c(pH1,pH2))
  }
}

