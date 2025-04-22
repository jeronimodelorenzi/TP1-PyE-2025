# _________________
# AGUA_CALIENTE_BAÑOS: Variable categórica en escala nominal.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Gráfico de barras de variable categórica en escala nominal: acceso a agua caliente en baños.
# Gráfico de barras de variable categórica en escala nominal: acceso a agua caliente en baños
datos_limpios %>%
  filter(!is.na(agua_caliente_baño)) %>%  # Eliminar filas con NA en agua_caliente_baño
  count(agua_caliente_baño) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ggplot(aes(
    x = reorder(agua_caliente_baño, -n),  # Ordenar por frecuencia
    y = porcentaje
  )) +
  geom_bar(
    stat = "identity",
    fill = "lightgray",
    col = "black",
    alpha = 0.6,
    width = 0.75
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje)),
    position = position_stack(vjust = 0.5),  # Centrar etiquetas
    color = "black",
    size = 4
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 50),  # Límites del eje Y: de 0% a 50%
    expand = c(0, 0)    # Elimina espacio extra en los extremos (opcional)
  ) +
  labs(
    y = "Porcentaje de viviendas", 
    x = "Acceso a agua caliente",
    title = "Acceso a sistemas de agua caliente en baños: % de hogares por tipo"
  )

