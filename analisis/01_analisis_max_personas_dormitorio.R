# _________________
# MAX_PERSONAS_DORMITORIO: Variable cuantitativa discreta.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

### Tabla de frecuencia según máxima cantidad de personas por dormitorio.
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


### Gráfico de bastones de máxima cantidad de personas por dormitorio.
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
       title = "Distribución de los hogares según cantidad de personas por dormitorio. Barrios populares de Argentina, año 2022.")


### Gráfico de torta de variable hacinamiento_dormitorio.
ggplot(datos_limpios, aes(x = "", fill = hacinamiento_dormitorio)) +
  geom_bar(aes(y = ..count../sum(..count..)), width = 1, color = "white") +
  coord_polar("y") +
  scale_fill_manual(name = "Hacinamiento", values = c("#7ed321", "#f1c40f", "#e74c3c")) +
  geom_text(
    stat = "count",
    aes(label = paste0(round(..count../sum(..count..)*100, 1), "%"),
        y = ..count../sum(..count..)),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 4
  ) +
  labs(title = "Proporción de hogares según nivel de hacinamiento (personas por dormitorio). Barrios populares de Argentina, año 2022.") +
  theme_void()


### Medidas estadísticas.
tabla_medidas_max_personas_dormitorio <- datos_limpios %>%
  summarise(
    Variable = "max_personas_dormitorio",
    Media = mean(max_personas_dormitorio),
    Mediana = median(max_personas_dormitorio),
    Moda = {
      freq <- table(max_personas_dormitorio)
      as.numeric(names(freq)[which.max(freq)])
    },
    Mínimo = min(max_personas_dormitorio),
    Máximo = max(max_personas_dormitorio),
    Rango = max(max_personas_dormitorio) - min(max_personas_dormitorio),
    Q1 = quantile(max_personas_dormitorio, 0.25),
    Q3 = quantile(max_personas_dormitorio, 0.75),
    Rango_Intercuartil = IQR(max_personas_dormitorio),
    Desvío = sd(max_personas_dormitorio),
    CV = (sd(max_personas_dormitorio) / mean(max_personas_dormitorio)) * 100
  )

tabla_medidas_max_personas_dormitorio
