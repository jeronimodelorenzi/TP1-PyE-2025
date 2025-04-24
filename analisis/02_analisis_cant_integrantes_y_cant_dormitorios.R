# _________________
# TIPO_TENENCIA y CANT_DORMITORIOS: Variable categórica nominal y cuantitativa discreta.
# Visualización, estadísticas y gráficos.

library(tidyverse)
attach(datos_limpios)

### Coeficiente de correlación de Pearson.
cor(datos_limpios$cant_integrantes, datos_limpios$cant_dormitorios, method = "pearson")

### Gráfico de dispersión básico
datos_limpios %>% 
  ggplot(aes(x=cant_integrantes, y=cant_dormitorios)) +
  geom_jitter(width=0.2, height=0.2, alpha=0.6, color="#4682B4", size=2) +
  scale_x_continuous(breaks=0:max(datos_limpios$cant_integrantes)) +
  scale_y_continuous(breaks=0:max(datos_limpios$menores_18)) +
  labs(x="Cantidad Integrantes", 
       y="Cantidad Dormitorios", 
       title = "Relación entre cantidad de integrantes y cantidad de dormitorios")