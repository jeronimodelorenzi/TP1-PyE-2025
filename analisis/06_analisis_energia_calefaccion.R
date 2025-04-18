# _________________
# ENERGIA_CALEFACCION: Variable de respuesta múltiple.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

# Visualizar datos.
datos_reducido1 <- datos_limpios %>%
  select(   # Seleccionar las columnas que quiero conservar
    energia_calefaccion_electricidad, 
    energia_calefaccion_garrafa, 
    energia_calefaccion_gas_natural, 
    energia_calefaccion_leña_carbon, 
    energia_calefaccion_sin, 
    energia_calefaccion_no_necesita
  )

# Vector con nombres para las barras.
vector_nombres_calefaccion <- c(
  energia_calefaccion_gas_natural = "Gas natural",
  energia_calefaccion_garrafa = "Garrafa",
  energia_calefaccion_electricidad = "Electricidad",
  energia_calefaccion_leña_carbon = "Leña / Carbón",
  energia_calefaccion_sin = "Sin calefacción",
  energia_calefaccion_no_necesita = "No necesita"
)

# Código con nombres. VERIFICAR PORCENTAJES.
datos_limpios %>%
  select(all_of(names(vector_nombres_calefaccion))) %>%
  summarise(across(everything(), ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "tipo_energia", values_to = "n") %>%
  mutate(
    tipo_energia_nuevo = vector_nombres_calefaccion[tipo_energia],
    porcentaje = n / sum(n) * 100
  ) %>%
  ggplot(aes(
    x = reorder(tipo_energia_nuevo, -n),
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
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 4
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 45),
    expand = c(0, 0)
  ) +
  labs(
    x = "Tipo de energía para calefacción",
    y = "Porcentaje de viviendas",
    title = "Distribución del uso de energía para calefacción"
  ) +
  theme_minimal()

# Conteo de registros para verificar.
conteo_1 <- sum(datos_limpios$energia_calefaccion_leña_carbon == 1, na.rm = TRUE)

# Total de registros no faltantes
total <- sum(!is.na(datos_limpios$max_personas_dormitorio))

# Porcentaje
(conteo_1 / total) * 100