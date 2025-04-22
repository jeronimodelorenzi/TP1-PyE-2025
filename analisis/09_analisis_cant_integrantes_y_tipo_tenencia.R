library(tidyverse)
attach(datos_limpios)

# Relación categórica discreta + cuantitativa discreta: tipo_tenencia + cantidad_personas.

ggplot(datos_limpios, aes(x = tipo_tenencia, y = cant_integrantes)) +
  geom_boxplot(show.legend = FALSE, fill = "lightblue") +
  labs(
    x = "Tipo de tenencia de la vivienda",
    y = "Cantidad de integrantes",
    title = "Distribución de integrantes por hogar según tipo de tenencia"
  ) +
  coord_flip() +  # Da vuelta los ejes para mejor visualización
  theme_light()
