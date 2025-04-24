# _________________
# TIPO_TENENCIA y CANT_DORMITORIOS: Variable categórica nominal y cuantitativa discreta.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

### Gráfico boxplot de la relación entre tipo_tenencia y cant_dormitorios.
ggplot(datos_limpios, 
       aes(x = fct_relevel(tipo_tenencia, # Orden de los boxplot.
                           "Otro",
                           "Ocupado",
                           "Prestado",
                           "Alquilado",
                           "Propio sin títulos",
                           "Propio con comprobante"), 
           y = cant_dormitorios)) +
  geom_boxplot(show.legend = FALSE, fill = "lightblue") +
  scale_y_continuous(
    breaks = seq(  # Secuencia de números enteros
      from = min(datos_limpios$cant_dormitorios, na.rm = TRUE),
      to = max(datos_limpios$cant_dormitorios, na.rm = TRUE),
      by = 1  # Incremento de 1 en 1
    ),
    labels = scales::number_format(accuracy = 1)
  ) +
  labs(
    x = "Tipo de tenencia de la vivienda",
    y = "Cantidad de dormitorios",
    title = "Distribución de la cantidad de dormitorios según tipo de tenencia de la vivienda. Barrios populares de Argentina, año 2022."
  ) +
  coord_flip()


### Medidas estadísticas.
tabla_medidas_tipo_tenencia <- datos_limpios %>%
  group_by(`tipo_tenencia`) %>%
  summarise(
    `Promedio de dormitorios` = mean(cant_dormitorios) %>% round(2),
    Mediana = median(cant_dormitorios),
    Mínimo = min(cant_dormitorios),
    Máximo = max(cant_dormitorios),
    Q1 = quantile(cant_dormitorios, 0.25),
    Q3 = quantile(cant_dormitorios, 0.75),
    Rango_Intercuartil = IQR(cant_dormitorios),
    Desvío = sd(cant_dormitorios),
    CV = (sd(cant_dormitorios) / mean(cant_dormitorios)) * 100
  ) %>%
  arrange(desc(`Promedio de dormitorios`))  # Ordena de mayor a menor promedio

tabla_medidas_tipo_tenencia
