#Graficar graficos
#importar librería para graficos
library(ggplot2)

#función para graficar

getGraph <- function(datos, values) {
  # Convertir los datos de ancho a largo
  datos_largo <- datos %>%
    pivot_longer(cols = -c(año, pp_max), names_to = "mes", values_to = "precipitacion")
  
  ggplot(datos, aes(x = año, y = pp_max)) +
    geom_point(color = "blue", size = 1) +
    geom_line(data = datos, aes(x = año, y = pp_max, color = "Precipitación")) +
    geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
    geom_line(data = datos, aes(x = año, y = values[2], color = "Precipitación Mínima")) +
    geom_line(data = datos, aes(x = año, y = values[1], color = "Precipitación Máxima")) +
    labs(x = "", 
         y = "Precipitación Max (mm)", 
         title = "Análisis de consistencia", 
         subtitle = "Histograma de precipitación anual histórica\n", 
         caption = "Source: SENAMHI") +
    scale_color_manual(
      values = c("Precipitación (mm)" = "blue", 
                 "Precipitación Mínima" = "purple", 
                 "Precipitación Máxima" = "orange")) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "bottom", 
          legend.direction = "horizontal",  # Establecer la dirección de la leyenda
          legend.title = element_blank(),     # Opcional: eliminar el título de la leyenda
          legend.key.width = unit(1, "cm"))  # Opcional: ajustar el ancho de las teclas de la leyenda
}