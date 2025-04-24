# _________________
# MATERIAL PARED + HUMEDAD: Relación de variables categóricas.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

### Mejorar datos.
materiales_resumidos <- c(
  "Mampostería (ladrillo/block" = "Mampostería",
  "Trama en madera/tapial de madera" = "Madera",
  "Chapa" = "Chapa",
  "Planchón (describir qué es)" = "Planchón",
  "Adobe" = "Adobe"
)

### Tabla de contigencia
tabla_contingencia_humedad_lugar <- datos_limpios %>%
  filter(
    humedad_lugar != "Ninguno",  
    !is.na(material_paredes_exteriores) 
  ) %>%
  mutate(material_resumido = materiales_resumidos[material_paredes_exteriores]) %>%
  tabyl(material_resumido, humedad_lugar) %>%  
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "row") %>% 
  adorn_pct_formatting(digits = 1) 

tabla_contingencia_humedad_lugar



