##Manipular tabla para darle forma
setTable <- function(datos){
  # Asignar nombres a las columnas
  colnames(datos) <- c("año", "mes", "dia", "precipitacion", "max_temp", "min_temp")
  # Limpiar datos con NA
  datos$precipitacion[datos$precipitacion < 0] <- NA
  datos$max_temp[datos$max_temp == -99.9] <- NA
  datos$min_temp[datos$min_temp == -99.9] <- NA
  # Ordenar el dataFrame en Precipitación año / mes
  library(dplyr)
  pp_by_month <- datos %>%
    group_by(año, mes) %>%
    summarise(pp_total = max(precipitacion))
  # Reorganizar el dataFrame en Precipitación año / meses (enero, ... , diciembre)
  library(tidyr)
  pp_by_month <- pp_by_month %>%
    pivot_wider(names_from = mes, values_from = pp_total) %>%
    rename(enero = `1`, febrero = `2`, marzo = `3`, abril = `4`, mayo = `5`, junio = `6`,
           julio = `7`, agosto = `8`, septiembre = `9`, octubre = `10`, noviembre = `11`, diciembre = `12`)
  # Ordenar las columnas para que empiezen desde enero y sigan en orden cronológico
  pp_by_month <- pp_by_month %>%
    select(año, enero, febrero, marzo, abril, mayo, junio, julio, agosto, septiembre, octubre, noviembre, diciembre)
  
  return(pp_by_month)
}

##Añadir precipitación maxima y logaritmo
addTail <-function(datos){
  #añadir precipitación maxima anual
  datos <- datos %>%
    rowwise() %>%
    mutate(pp_max = max(enero, febrero, marzo, abril, mayo, junio, julio, agosto, septiembre, octubre, noviembre, diciembre, na.rm = TRUE))
  
  #añadir precipitación logaritmica maxima anual
  datos <- datos %>%
    rowwise() %>%
    mutate(pp_log = log10(pp_max))
}