# _________________
# ENERGIA_CALEFACCION: Variable de respuesta múltiple.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

### Visualizar datos.
datos_reducido1 <- datos_limpios %>%
  select(   # Seleccionar las columnas que quiero conservar
    energia_calefaccion_electricidad, 
    energia_calefaccion_garrafa, 
    energia_calefaccion_gas_natural, 
    energia_calefaccion_leña_carbon, 
    energia_calefaccion_sin, 
    energia_calefaccion_no_necesita
  )

# View(datos_reducido1)

### Vector con nombres para las barras.
vector_nombres_calefaccion <- c(
  energia_calefaccion_gas_natural = "Gas natural",
  energia_calefaccion_garrafa = "Garrafa",
  energia_calefaccion_electricidad = "Electricidad",
  energia_calefaccion_leña_carbon = "Leña / Carbón",
  energia_calefaccion_sin = "Sin calefacción",
  energia_calefaccion_no_necesita = "No necesita"
)

# Total de viviendas
total_viviendas <- nrow(datos_limpios)

### Gráfico de barras de distribución del uso de energía para calefacción.
datos_limpios %>%
  select(all_of(names(vector_nombres_calefaccion))) %>%
  summarise(across(everything(), ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "tipo_energia", values_to = "n") %>%
  mutate(
    tipo_energia_nuevo = vector_nombres_calefaccion[tipo_energia],
    porcentaje = n / total_viviendas * 100
  ) %>%
  ggplot(aes(
    x = reorder(tipo_energia_nuevo, -porcentaje),
    y = porcentaje
  )) +
  geom_bar(
    stat = "identity",
    fill = "steelblue",
    col = "black",
    alpha = 0.6,
    width = 0.75
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje)),
    vjust = -0.5,
    color = "black",
    size = 4
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Tipo de energía para calefacción",
    y = "Porcentaje de viviendas",
    title = "Uso de fuentes de energía para calefacción (porcentaje de hogares)"
  )
