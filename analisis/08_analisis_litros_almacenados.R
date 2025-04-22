# _________________
# LITROS_ALMACENADOS: Variable cuantitativa continua.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

tabla_litros <- datos_limpios %>%
  filter(!is.na(litros_intervalo)) %>%
  group_by(litros_intervalo) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 1))

print(tabla_litros)

ggplot(tabla_litros, aes(x = litros_intervalo, y = porcentaje, fill = litros_intervalo)) +
  geom_col(color = "black", width = 0.7) +
  geom_text(aes(label = paste0(porcentaje, "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title = "Distribución de litros de agua almacenados",
    x = "Intervalos de litros almacenados",
    y = "Porcentaje de hogares"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )
