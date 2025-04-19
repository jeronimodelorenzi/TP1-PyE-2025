# _________________
# MAX_PERSONAS_DORMITORIO: Variable cuantitativa discreta.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Histograma de máxima cantidad de integrantes por dormitorio.
ggplot(datos_limpios) +
  aes(x = max_personas_dormitorio, y = ..count../sum(..count..)) +
  geom_histogram(fill = "lightgray",
                 col = "black",
                 breaks = seq(0, max(datos_limpios$max_personas_dormitorio, na.rm = TRUE) + 1, 1)) +
  scale_x_continuous(breaks = function(x) seq(0, max(x), by = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Máximo de personas por dormitorio", 
       y = "Frecuencia relativa (%)",
       title = "Distribución de personas por dormitorio")
