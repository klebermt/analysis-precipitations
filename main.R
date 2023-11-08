##Analisís de datos dudosos
#importar las funciones
source("tabla.R")
source("Completar datos.R")
source("clean.R")
source("analisis de datos dudosos.R")

#importar los datos
datos <- read.csv("data/pucara.csv", header = FALSE, sep = ' ')

#Damos forma a la tabla
datos <- setTable(datos)

#Verificamos datos nulos
library(mice)
md.pattern(datos, rotate.names = TRUE)

#limpiamos los datos
datos.cleaned <- clean(datos)

#verificamos datos nulos de nuevo
md.pattern(datos.cleaned, rotate.names = TRUE)

#completamos datos faltantes
datos.fill <- fillInter(datos.cleaned)
datos.fill2 <- fillMean(datos.cleaned)

#agregar maximos totales y logaritmos
datos.fill <- addTail(datos.fill)
datos.fill2 <- addTail(datos.fill2)

#calcular el valores atipicos
values.atipicos <- getXH(datos.fill)
values.atipicos2 <- getXH(datos.fill2)


#graficamos
getGraph(datos.fill, values.atipicos)
getGraph(datos.fill2, values.atipicos2)

#ajustar los datos
datos.fill.re <- reclean(datos.fill2, values.atipicos2)
source("graphics.R")
getGraph(datos.fill.re, values.atipicos2)
