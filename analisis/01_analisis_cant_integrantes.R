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


# Gráfico de bastones de cantidad de integrantes por vivienda.
ggplot(datos_limpios) +
  aes(x = cant_integrantes, y = ..count../sum(..count..)) +
  geom_bar(width = 0.1, fill = 'steelblue') +
  scale_x_continuous(breaks = function(x) seq(0, max(x), by = 1)) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.25),  # Límite del 25%
    expand = c(0, 0)       # Sin espacio extra en ejes
  )  +
  stat_count( # Texto con porcentajes
    aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)),
    geom = "text",
    vjust = -1,
    color = "black",
    size = 3.5
  ) +
  labs(x = "Cantidad de integrantes", 
       y = "Porcentaje de hogares (%)",
       title = "Distribución de integrantes por hogar")


# Gráfico escalonado de frecuencias acumuladas de cantidad de integrantes por vivienda.
# Preparamos los datos acumulados.
datos_acumulados <- datos_limpios %>%
  count(cant_integrantes) %>%
  arrange(cant_integrantes) %>%
  mutate(
    frecuencia_relativa = n / sum(n),
    frecuencia_acumulada = cumsum(frecuencia_relativa)
  )

# Gráfico escalonado.
ggplot(datos_acumulados) +
  aes(x = cant_integrantes, y = frecuencia_acumulada) +
  geom_step(
    direction = "hv",  # Horizontal-vertical (escalones rectos)
    color = "steelblue",
    linewidth = 1.2
  ) +
  geom_point(  # Añade puntos en cada paso
    size = 3,
    color = "steelblue"
  ) +
  geom_text(
    aes(label = scales::percent(frecuencia_acumulada)),
    vjust = -1,
    color = "black",
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = function(x) seq(0, max(x), by = 1)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1.05)
  ) +
  labs(
    x = "Cantidad de integrantes", 
    y = "Porcentaje acumulado de hogares (%)",
    title = "Distribución acumulada de integrantes por hogar",
    subtitle = "Porcentaje acumulado de hogares con X integrantes o menos"
  )


# Medidas estadísticas.
mean(cant_integrantes) # Media aritmética
median(cant_integrantes) # Mediana
sort(table(cant_integrantes), decreasing = TRUE)[1]  # Moda
