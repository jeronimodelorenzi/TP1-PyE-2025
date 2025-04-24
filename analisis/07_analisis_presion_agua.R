# _________________
# PRESION_AGUA: Variable categórica ordinal.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

### Tabla de frecuencia. Distribución de la presión de agua en hogares
tabla_frecuencia_presion_agua = 
  tabyl(datos_limpios$presion_agua) %>% 
  rename(   # Renombro columnas
    "Presión del agua" = "datos_limpios$presion_agua",
    "Frecuencia Absoluta" = n,
    "Frecuencia Relativa" = percent
  ) %>% 
  adorn_totals() %>%  # Agrego fila de totales
  adorn_pct_formatting(digits = 1)

tabla_frecuencia_presion_agua


### Gráfico de barras de presión del agua en viviendas.
# 1. Definir orden y calcular frecuencias/porcentajes.
datos_grafico <- datos_limpios %>%
  count(presion_agua) %>%
  mutate(
    presion_agua = factor(presion_agua, levels = c("Buena", "Débil", "Muy débil")),
    porcentaje = n / sum(n) * 100
  ) %>%
  arrange(presion_agua)


# 2. Crear gráfico de barras
ggplot(datos_grafico, aes(x = presion_agua, y = porcentaje)) +
  geom_col(width = 0.4, color = "black", fill = "steelblue") +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje)),
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) +
  scale_y_continuous(limits = c(0, 50)) + # Límite del 50%
  labs(
    title = "Distribución de la presión del agua en hogares",
    x = "Nivel de presión",
    y = "Porcentaje de hogares"
  )


# Medidas estadísticas.
# 1. Asegurar que la variable sea un factor ordenado.
datos_limpios <- datos_limpios %>%
  mutate(
    presion_agua = factor(
      presion_agua,
      levels = c("Buena", "Débil", "Muy débil"),  # Orden de mejor a peor
      ordered = TRUE
    )
  )

# 2. Calcular la mediana.
mediana_presion <- datos_limpios$presion_agua[median(as.numeric(datos_limpios$presion_agua))] %>% 
  as.character()

mediana_presion
