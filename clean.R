clean <- function(datos){
  #creamos un array donde estén las filas que más de 4 datos nulos
  delete.data <- c()
  
  #recorremos todos las filas
  for (i in 1:nrow(datos)) {
    #seleccionamos la fila sin el año y contamos cuantos valores no nulos tiene
    n <- sum(!is.na(datos[i, 2:13]))
    #si una fila tiene menos de 8 valores nulos será eliminada
    if (n < 8) {
      #guardamos la fila como menos de 8 valores nulos
      delete.data <- c(delete.data, i)
    } 
  }
  #Eliminamos las filas
  datos <- datos[-delete.data, ]
  return(datos)
}

reclean <- function(datos, values){
  #Eliminar valores bajos
  #creamos un array donde estén las filas con datos menores que los limites
  delete.data <- c()
  
  #limpiar valores bajos
  for (i in 1:nrow(datos)) {
    if(datos$pp_max[i] < values[2]) {
      delete.data <- c(delete.data, i)
    }
  }
  
  #Eliminar valores altos
  for (i in 1:nrow(datos)){
    for (j in 2:ncol(datos)){
      
      #comprobamos si los datos son mayores que el limite
      if(datos[i,j] > values[1]){
        
        #cambiamos el valor a 0
        datos[i,j] <- 0
      }
    }
  }
  
  # Comprobamos que no hay datos para cambiar
  if (is.null(delete.data)) {
    print("no hay datos para corregir")
    return(datos)
  } else {
    datos <- datos[-delete.data, ]
  }
  
  return(datos)
}