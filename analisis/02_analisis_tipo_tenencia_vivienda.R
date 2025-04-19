# _________________
# TIPO_TENENCIA: Variable categórica nominal.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Medidas resumen por grupo a partir del tipo de vivienda.
datos_limpios %>% group_by(tipo_tenencia) %>%
  summarize(cant_integrantes_media = mean(cant_integrantes),
            cant_integrantes_ds = sd(cant_integrantes))

# Gráfico de barras de tipo de tenencia de la vivienda.
datos_limpios %>%
  count(tipo_tenencia) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ggplot(aes(
    x = reorder(tipo_tenencia, -n),  # Ordenar por frecuencia
    y = porcentaje
  )) +
  geom_bar(
    stat = "identity",
    fill = "lightgray",
    col = "black",
    alpha = 0.6,
    width = 0.75
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje)),
    position = position_stack(vjust = 0.5),  # Centrar etiquetas
    color = "black",
    size = 4
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 50),  # Límites del eje Y: de 0% a 50%
    expand = c(0, 0)    # Elimina espacio extra en los extremos (opcional)
  ) +
  labs(
    y = "Porcentaje de viviendas", 
    x = "Tipo de tenencia",
    title = "Distribución del tipo de tenencia de vivienda"
  )
