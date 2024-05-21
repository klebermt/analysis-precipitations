##Analisís de datos dudosos
#importar las funciones
source("tabla.R")
source("Completar datos.R")
source("clean.R")
source("analisis de datos dudosos.R")
source("graphics.R")

#importar los datos
datos <- read.csv("data/EL PINTOR.csv", header = FALSE, sep = ' ')

get_analysis <- function (datos) {
  #Damos forma a la tabla
  datos <- setTable(datos)
  
  #limpiamos los datos
  datos.cleaned <- clean(datos)
  
  #completamos datos faltantes
  datos.fill <- fillInter(datos.cleaned) # Competación de datos por interpolación
  # datos.fill2 <- fillMean(datos.cleaned) # completacioón de datos con la media
  
  #agregar maximos totales y logaritmos
  datos.fill <- addTail(datos.fill)
  # datos.fill2 <- addTail(datos.fill2) # datos de la media
  
  #calcular el valores atipicos
  values.atipicos <- getXH(datos.fill)
  # values.atipicos2 <- getXH(datos.fill2) # datos de la media
  
  #graficamos
  print(getGraph(datos.fill, values.atipicos))
  # getGraph(datos.fill2, values.atipicos2) # datos de la media
  
  # Devolver los datos y valores atípicos como una lista
  return(list(data = datos.fill, atipicos = values.atipicos))
}

# Ejecución
result <- get_analysis(datos)
data <- result$data
atipicos <- result$atipicos

# Si los datos pasan los límites hay que ajustar los datos
data <- reclean(data, atipicos) # si es que hay datos dudosos
getGraph(data, atipicos)

# Obtener el promedio de presipitación anual
data$mean <- rowMeans(data[2:13])
mean.anual <- mean(data$mean)
print(mean.anual)
