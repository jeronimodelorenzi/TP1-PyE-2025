# _________________
# COSTO_ALQUILER: Variable cuantitativa continua.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Tabla de intervalo de precios alquiler
tabla_intervalos <- datos_limpios %>% filter(!is.na(costo_alquiler)) %>%
  group_by(costo_alquiler) %>%
  summarize(cantidad = n())

# Histograma de costo de alquiler por vivienda

ggplot(datos_limpios) +
  aes(x = costo_alquiler_num, y = ..count../sum(..count..)) +
  geom_histogram(
    fill = "lightgray", 
    col = "black",
    breaks = seq(0, 30000, by = 2000)
  ) +
  scale_x_continuous(
    breaks = seq(0, 30000, by = 5000),
    labels = scales::dollar_format(prefix = "$", big.mark = ".")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Costo de alquiler",
    y = "Frecuencia relativa (%)",
    title = "Distribución del costo de alquiler"
  )

# Media

media_costo_alquiler <- mean(datos_limpios$costo_alquiler_num, na.rm = TRUE)
mediana_costo_alquiler <- median(datos_limpios$costo_alquiler_num, na.rm = TRUE)
desvio_costo_alquiler <- sd(datos_limpios$costo_alquiler_num, na.rm = TRUE)
