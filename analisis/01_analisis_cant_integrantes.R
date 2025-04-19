# _________________
# CANT_INTEGRANTES: Variable cuantitativa discreta.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Tabla de frecuencia según cantidad de integrantes de la familia.
tabla_frecuencia_cant_integrantes = 
  tabyl(datos_limpios$cant_integrantes) %>% 
  rename(   # Renombro columnas
    "Cant. Integrantes" = "datos_limpios$cant_integrantes",
    "Frecuencia Absoluta" = n,
    "Frecuencia Relativa" = percent
  ) %>% 
  adorn_totals() %>%  # Agrego fila de totales
  adorn_pct_formatting(digits = 1) # Cant. de decimales en %

tabla_frecuencia_cant_integrantes

# Histograma de cantidad de integrantes por vivienda.
ggplot(datos_limpios) +
  aes(x = cant_integrantes, y = ..count../sum(..count..)) +
  geom_histogram(fill = "lightgray", 
                 col = "black",
                 breaks = seq(0, max(datos_limpios$cant_integrantes, na.rm = TRUE) + 1, 1)) +
  scale_x_continuous(breaks = function(x) seq(0, max(x), by = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Cantidad de integrantes", 
       y = "Frecuencia relativa (%)",
       title = "Distribución de integrantes por hogar")
