library(dplyr)
# Establecemos la tabla de valores kn conocidos
tabla_valores_kn_conocidos <- data.frame(
  muestra = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
              31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 55,
              60, 65, 70, 75, 80, 85, 90, 95, 100, 110, 120, 130, 140),
  kn = c(2.036, 2.088, 2.134, 2.175, 2.213, 2.247, 2.279, 2.309, 2.335, 2.361, 2.385, 2.408,
         2.429, 2.448, 2.467, 2.486, 2.502, 2.190, 2.534, 2.549, 2.563, 2.577, 2.591, 2.604,
         2.616, 2.628, 2.639, 2.650, 2.661, 2.671, 2.682, 2.692, 2.700, 2.710, 2.719, 2.727,
         2.736, 2.744, 2.753, 2.760, 2.768, 2.804, 2.837, 2.866, 2.893, 2.917, 2.940, 2.961,
         2.981, 3.000, 3.017, 3.049, 3.078, 3.104, 3.129)
)

## Función para interpolar valores faltantes en la tabla de valores kn
interpolarValoresFaltantes <- function(tabla_conocidos, muestra_min, muestra_max) {
  # Filtrar los valores conocidos dentro del rango deseado
  tabla_filtrada <- tabla_conocidos %>%
    filter(muestra >= muestra_min & muestra <= muestra_max)
  
  # Realizar la interpolación lineal para estimar los valores desconocidos
  valores_interp <- approxfun(tabla_filtrada$muestra, tabla_filtrada$kn)
  
  # Crear un nuevo data frame con las muestras y los valores interpolados
  muestras_interp <- seq(muestra_min, muestra_max, by = 1)
  valores_interp <- valores_interp(muestras_interp)
  
  tabla_interp <- data.frame(
    muestra = muestras_interp,
    kn = valores_interp
  )
  
  return(tabla_interp)
}

##Función para obtener el valor kn para un tamaño de muestra dado
obtenerValorKn <- function(tamano_muestra, tabla) {
  valor_kn <- tabla$kn[match(tamano_muestra, tabla$muestra)]
  return(valor_kn)
}

##Interpolamos la tabla Kn
setValueKn <- function(muestra){
  # Definir el rango de muestras para interpolar
  muestra_min <- 50
  muestra_max <- 140
  
  # Realizar la interpolación
  tabla_valores_interp <- interpolarValoresFaltantes(tabla_valores_kn_conocidos, muestra_min, muestra_max)
  
  # Filtrar los valores de la tabla original para el rango del 10 al 50
  tabla_original_filtrada <- tabla_valores_kn_conocidos %>%
    filter(muestra >= 10 & muestra <= 49)
  
  # Combinar la tabla original con la tabla interpolada
  tabla_completa <- bind_rows(tabla_original_filtrada, tabla_valores_interp)
  tabla_completa$kn <- round(tabla_completa$kn, digits = 3)
  
  # Obtén el valor kn correspondiente
  valor_kn <- obtenerValorKn(muestra, tabla_completa)
  
  return(valor_kn)
}