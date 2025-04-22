# _________________
# MAX_PERSONAS_DORMITORIO: Variable cuantitativa discreta.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Tabla de frecuencia según máxima cantidad de personas por dormitorio.
tabla_frecuencia_max_personas_dormitorio = 
  tabyl(datos_limpios$max_personas_dormitorio) %>% 
  rename(   # Renombro columnas
    "Cant. Integrantes" = "datos_limpios$max_personas_dormitorio",
    "Frecuencia Absoluta" = n,
    "Frecuencia Relativa" = percent
  ) %>% 
  adorn_totals() %>%  # Agrego fila de totales
  adorn_pct_formatting(digits = 1) # Cant. de decimales en %

tabla_frecuencia_max_personas_dormitorio


# Gráfico de bastones de máxima cantidad de personas por dormitorio.
ggplot(datos_limpios) +
  aes(x = max_personas_dormitorio, 
      y = ..count../sum(..count..)) +
  geom_bar(width = 0.1, fill = 'steelblue') +
  scale_x_continuous(breaks = function(x) seq(0, max(x), by = 1)) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.45),  # Límite del 45%
  )  +
  stat_count( # Texto con porcentajes
    aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)),
    geom = "text",
    vjust = -1,
    color = "black",
    size = 3.5
  ) +
  labs(x = "Máx. personas por dormitorio", 
       y = "Porcentaje de hogares (%)",
       title = "Distribución de personas por dormitorio")


# Gráfico de torta de variable hacinamiento_dormitorio.
ggplot(datos_limpios, aes(x = "", fill = hacinamiento_dormitorio)) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y") +
  scale_fill_manual(values = c("#7ed321", "#f1c40f", "#e74c3c")) +
  labs(title = "Niveles de hacinamiento") +
  theme_void()



# Gráfico de bastones de máxima cantidad de personas por dormitorio y condición de hacinamiento.
ggplot(datos_limpios, 
       aes(x = max_personas_dormitorio,
           y = ..count../sum(..count..),
           fill = hacinamiento_dormitorio)) +
  geom_bar(width = 0.1, stat = "count") +
  scale_x_continuous(breaks = function(x) seq(0, max(x, na.rm = TRUE), by = 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.45)) +
  scale_fill_manual(name = "Hacinamiento", values = c("Sin hacinamiento" = "#7ed321", "Moderado" = "#f1c40f", "Crítico" = "#e74c3c")) +
  # Texto con porcentajes
  geom_text( 
    stat = "count",
    aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)),
    vjust = -1,
    size = 3.5,
  ) +
  labs(
    x = "Máx. personas por dormitorio", 
    y = "Porcentaje de hogares (%)",
    title = "Distribución de máxima cantidad de personas por dormitorio y nivel de hacinamiento"
  )


