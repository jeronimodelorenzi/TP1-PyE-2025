# _________________
# PRESION_AGUA: Variable categórica ordinal.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Gráfico de barras de variable categórica ordinal: presión del agua en viviendas.
# 1. Definir orden lógico de las categorías (de mejor a peor presión)
niveles_ordenados <- c("Buena", "Débil", "Muy débil")

# 2. Calcular frecuencias y porcentajes
datos_grafico <- datos_limpios %>%
  count(presion_agua) %>%
  mutate(
    presion_agua = factor(presion_agua, levels = niveles_ordenados),
    porcentaje = n / sum(n) * 100
  ) %>%
  arrange(presion_agua)  # Ordenar según niveles definidos

# 3. Gráfico de barras ordenado
ggplot(datos_grafico, aes(x = presion_agua, y = porcentaje, fill = presion_agua)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje)),
    vjust = -0.5,  # Posición arriba de la barra
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Buena" = "#4CAF50", "Débil" = "#FFC107", "Muy débil" = "#F44336"),
    guide = "none"  # Oculta la leyenda de colores
  ) +
  labs(
    title = "Distribución de la presión del agua en hogares",
    x = "Nivel de presión",
    y = "Porcentaje de hogares"
  ) +
  theme(
    axis.text.x = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

