##Método del promedio aritmético
fillMean <- function(datos) {
  for (i in 1:nrow(datos)) {
    for (j in 2:ncol(datos)) {  # Comienza desde la segunda columna (enero) hasta la última (diciembre)
      if (is.na(datos[i, j])) {  # Comprueba si el valor es NA
        promedio <- colMeans(datos[ ,j, drop = FALSE], na.rm = TRUE)
        # Asigna el promedio calculado al valor faltante
        datos[i, j] <- round(promedio, digits = 1)
      }
    }
  }
  return(datos)
}

##Método de la interpolación
fillInter <- function(datos) {
  for (i in 1:nrow(datos)) {
    for (j in 2:ncol(datos)) {  # Comienza desde la segunda columna (enero) hasta la última (diciembre)
      if (is.na(datos[i, j])) {  # Comprueba si el valor es NA
        y0 <- min(datos[ ,j], na.rm = TRUE) #valor maximo en la columna
        y1 <- max(datos[ ,j], na.rm = TRUE) #valor minimo en la columna
        x <- datos$año[j] #seleccionamos el año faltante X
        x0 <- min(datos$año, na.rm = TRUE) #Año con la minima precipitación
        x1 <- max(datos$año, na.rm = TRUE) #Año con la maxima precipitación
        #formula de interpolación lineal
        yx <- y0 + (((y1-y0)/(x1-x0))*(x-x0))
        # Asigna el promedio calculado al valor faltante
        datos[i, j] <- round(yx, digits = 1)
      }
    }
  }
  return(datos)
}