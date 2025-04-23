# _________________
# TIPO_TENENCIA y CANT_DORMITORIOS: Variable categórica nominal y cuantitativa discreta.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Gráfico boxplot de la relación entre tipo_tenencia y cant_dormitorios.
ggplot(datos_limpios, 
       aes(x = fct_relevel(tipo_tenencia, # Orden de los boxplot.
                           "Otro",
                           "Ocupado/Tomado",
                           "Prestado",
                           "Alquilado",
                           "Propio sin títulos",
                           "Propio con algún comprobante de tenencia"), 
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
    title = "Distribución de cantidad de dormitorios según tipo de tenencia"
  ) +
  coord_flip()


# Medidas estadísticas.
tabla_resumen <- datos_limpios %>%
  group_by(`tipo_tenencia`) %>%
  summarise(
    `Promedio de dormitorios` = mean(cant_dormitorios, na.rm = TRUE) %>% round(2),
    Mediana = median(cant_dormitorios, na.rm = TRUE),
    Mínimo = min(cant_dormitorios, na.rm = TRUE),
    Máximo = max(cant_dormitorios, na.rm = TRUE)
  ) %>%
  arrange(desc(`Promedio de dormitorios`))  # Ordena de mayor a menor promedio

tabla_resumen
