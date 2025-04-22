library(tidyverse)
attach(datos_limpios)

# Relación categórica discreta + categórica discreta: material_pared + humedad

materiales_resumidos <- c(
  "Mampostería (ladrillo/block" = "Mampostería",
  "Trama en madera/tapial de madera" = "Madera",
  "Chapa" = "Chapa",
  "Planchón (describir qué es)" = "Planchón",
  "Adobe" = "Adobe"
)

datos_limpios %>%
  filter(
    rowSums(
      !is.na(select(., humedad_cocina, humedad_living, humedad_baño, humedad_dormitorios))
    ) > 0,
    !is.na(material_paredes_exteriores)
  ) %>%
  mutate(material_resumido = materiales_resumidos[material_paredes_exteriores]) %>%
  group_by(material_resumido) %>%
  summarise(cantidad_con_humedad = n()) %>%
  arrange(desc(cantidad_con_humedad)) %>%
  ggplot(aes(x = reorder(material_resumido, -cantidad_con_humedad), y = cantidad_con_humedad)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(
    x = "Material de paredes exteriores",
    y = "Cantidad de viviendas con humedad",
    title = "Viviendas con humedad según tipo de pared"
  ) +
  theme_minimal()

# Tabla de contigencia

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

# Mostrar la tabla
tabla_contingencia_humedad_lugar



