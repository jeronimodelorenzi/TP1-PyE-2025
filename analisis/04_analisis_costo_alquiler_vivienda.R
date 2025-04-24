# _________________
# COSTO_ALQUILER: Variable cuantitativa continua.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

### Filtrar datos que tengan valores en la columna costo_alquiler.
datos_alquiler <- datos_limpios %>%
  filter(!is.na(costo_alquiler_num))

### Tabla de intervalos de precios alquiler
tabla_intervalos_alquiler <- datos_alquiler %>%
  group_by(costo_alquiler) %>%
  summarise(
    f_i = n(),  # Frecuencia absoluta (número de observaciones en cada intervalo)
    h_i = (n() / nrow(datos_alquiler) )*100 # Frecuencia relativa (porcentaje sobre el total de alquileres)
  ) %>%
  mutate(
    F_i = cumsum(f_i),  # Frecuencia acumulada
    H_i = cumsum(h_i)  # Frecuencia relativa acumulada
  )

tabla_intervalos_alquiler

### Porcentaje entre $8.000 y $16.000.
mean(datos_alquiler$costo_alquiler_num >= 8000 & datos_alquiler$costo_alquiler_num <= 16000) * 100


### Histograma de costo de alquiler por vivienda.
ggplot(datos_alquiler) +
  aes(x = costo_alquiler_num, y = ..count../sum(..count..)) +
  geom_histogram(
    fill = "lightblue", 
    col = "black",
    breaks = seq(0, 30000, by = 2000)
  ) +
  scale_x_continuous(
    breaks = seq(0, 30000, by = 2000),
    labels = scales::dollar_format(prefix = "$", big.mark = ".")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Costo de alquiler",
    y = "Porcentaje de hogares (%)",
    title = "Distribución del costo mensual de alquiler en hogares inquilinos. Barrios populares de Argentina, año 2022."
  )


### Medidas estadísticas: media, mediana, desvío estándar y rango.
mean(datos_alquiler$costo_alquiler_num)
median(datos_alquiler$costo_alquiler_num)
sd(datos_alquiler$costo_alquiler_num)
max(datos_alquiler$costo_alquiler_num) - min(datos_alquiler$costo_alquiler_num)
