# IINSTALACIÓN DE PAQUETES E INCLUSIÓN DE LIBRERÍAS
# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googledrive")
# install.packages("readxl")
# install.packages("tidyverse")

library(googledrive)
library(readxl)

# ___________________________________________
# ADJUNTAR DATASET

# Descargo el archivo mediante su id de google drive
# El link de los archivos de drive tiene esta forma:
# https://docs.google.com/spreadsheets/d/1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy
# El id de esta hoja de cálculo es "1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"
googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), 
                            overwrite = T)

# Cargo el archivo como .xlsx
datos = readxl::read_excel("Datos_LP.xlsx", 
                           col_names = TRUE, 
                           skip = 2)

# Ver la estructura del dataset.
str(datos)
ncol(datos)
nrow(datos)

# Modificar nombre de las columnas.
colnames(datos) = c("id", "provincia", "barrio",
                    "edad_jefe_hogar","años_residencia","cant_integrantes","cant_familias", "cant_varones","cant_mujeres","cant_disidentes", "menores_18", "personas_discapacidad",
                    "cant_dormitorios", "max_personas_dormitorio",
                    "certificado_renabap", "intento_desalojo", "cant_desalojos", "años_ultimo_desalojo", "tipo_tenencia", "contrato_alquiler", "costo_alquiler", "aumento_alquiler", "porcentaje_aumento_alquiler",
                    "origen_agua", "compra_agua_embotellada", "presion_agua", "agua_en_altura", "litros_almacenados", "posee_baño", "higiene_fuera_vivienda", "baño_compartido", "baño_descarga", "tipo_desagüe", "agua_cocina", "agua_caliente_cocina", "agua_baño", "agua_caliente_baño",
                    "energia_cocina_gas_natural", "energia_cocina_garrafa", "energia_cocina_electricidad", "energia_cocina_leña_carbon", "energia_cocina_sin", "energia_calefaccion_gas_natural", "energia_calefaccion_garrafa", "energia_calefaccion_electricidad", "energia_calefaccion_leña_carbon", "energia_calefaccion_sin", "energia_calefaccion_no_necesita", "ventilacion_calefaccion",
                    "conexion_electricidad", "tendido_electrico", "perdida_electrodomesticos_electricidad", "incendios_electricidad", "frecuencia_corte_electricidad_verano","frecuencia_corte_electricidad_invierno", 
                    "banda_ancha", "celular_internet", "abonos_moviles", "computadoras", "telefonos",
                    "contrapiso", "material_piso", "material_techo", "aislamiento_techo", "material_puerta_cemento", "material_puerta_madera", "material_puerta_ceramico", "material_puerta_sin", "material_paredes_exteriores", "terminacion_exterior", "tipo_terrminacion_exterior", "terminacion_pintura", "humedad_dormitorios", "humedad_cocina", "humedad_baño", "humedad_living", "humedad_sin", "humedad_otro", "derrumbe_dormitorios", "derrumbe_cocina", "derrumbe_baño", "derrumbe_living", "derrumbe_sin", "derrumbe_otro", "trabajo_vivienda", "tipo_trabajo_vivienda",
                    "calle_asfaltqada", "salida_calle", "vereda_calle", "alumbrado_publico", "calificacion_arbolado", "plagas", "plagas_cucarachas", "plagas_mosquitos", "plagas_ratas", 
                    "esparcimiento_polideportivo", "esparcimiento_natatorio", "esparcimiento_playon", "esparcimiento_futbol", "esparcimiento_ejercicio", "esparcimiento_skatepark", "esparcimiento_balneario", "esparcimiento_sin", "esparcimiento_otro", "frecuencia_esparcimiento", "verdes_placita", "verdes_plaza", "verdes_parque", "verdes_sin", "frecuencia_verdes", "frecuencia_colectivo", "frecuencia_colectivo_dispar", "acceso_bici_publica", "basurales", "cesto_cuadra", "eliminacion_residuos", "recoleccion_residuos_municipio", "riesgo_inundacion"
)

# ___________________________________________
# VARIABLES
# 1.4: cant_integrantes - Cuantitativa discreta.
# 3.2: max_personas_dormitorio - Cuantitativa discreta.
# 4.3: tipo_tenencia - Categórica nominal.
# 4.4.2: costo_alquiler: Cuantitativa continua (intervalo).
# 5.3: presion_agua - Categórica ordinal.
# 5.16: agua_caliente_baño - Categórica nominal.
# 5.2.2: litros_almacenados - Cuantitativa continua.
# 6.2: energia_vivienda - Categórica de respuesta múltiple.
# 9.5: material_puertas - Categórica de respuesta múltiple.
# 9.6: material_paredes - Categórica discreta
# 9.9: humedad - Categórica de respuesta múltiple
# alguna de basura: 11.8

# Relación categórica discreta + cuantitativa discreta: tipo_tenencia + cantidad_personas.
# Relación categórica discreta + categórica discreta: material_pared + humedad
# Relación cuantitativa + cuantitativa: cant_integrantes + litros_almacenados

# Tipo de piso/tiene vereda + inundación (11.9)

# ___________________________________________
# LIMPIEZA DE DATOS
library(tidyverse)

maximo_año_residencia = max(datos$años_residencia)

# Limpieza de datos.
# - Pasar valores NA a 0 (asumimos NA como ausencia del atributo).
# - Especificar ordinalidad a las categorías de una variable.
datos_limpios <- datos %>% 
  mutate(cant_desalojos = ifelse(is.na(cant_desalojos),0,cant_desalojos),
         años_residencia = cut(años_residencia,
                               breaks = c(-1, 0, 1, 5, 10, 20, 40, 60, maximo_año_residencia),
                               labels = c("0 años (recién llegado)", 
                                          "1 año", 
                                          "2-5 años", 
                                          "6-10 años", 
                                          "11-20 años", 
                                          "21-40 años",
                                          "41-60 años",
                                          "61+ años"),
                               right = TRUE,
                               ordered_result = TRUE))

# Alquiler

#cantidad_alquiler <- as.numeric(sum(datos$tipo_tenencia == "Alquilado"))
#cantidad_intervalos <- round(sqrt(cantidad_alquiler))
#minimo_alquiler <- min(datos$costo_alquiler, na.rm = TRUE)
#maximo_alquiler <- max(datos$costo_alquiler, na.rm = TRUE)
#ancho <- ceiling((maximo_alquiler - minimo_alquiler) / cantidad_intervalos)

breaks_alquiler <- seq(from = 0, to = 30000, by = 5000)

labels_alquiler <- paste0(
  head(breaks_alquiler), " - ", tail(breaks_alquiler)
)

datos_limpios <- datos %>%
  mutate(costo_alquiler = cut(costo_alquiler,
                              breaks = breaks_alquiler,
                              labels = labels_alquiler,
                              right = TRUE,  
                              ordered_result = TRUE)
  )


# Humedad

cantidad_humedad_baño <- as.numeric(sum(datos$humedad_baño == "Baño", na.rm = TRUE))
cantidad_humedad_cocina <- as.numeric(sum(datos$humedad_cocina == "Cocina", na.rm = TRUE))
cantidad_humedad_dormitorios <- as.numeric(sum(datos$humedad_dormitorios == "Dormitorios", na.rm = TRUE))
cantidad_humedad_living <- as.numeric(sum(datos$humedad_living == "Living", na.rm = TRUE))
cantidad_sin_humedad <- as.numeric(sum(datos$humedad_sin == "No hay ningún problema de filtraciones/humedad", na.rm = TRUE))
cantidad_humedad_otro <- as.numeric(sum(datos$humedad_otro == "Otro", na.rm = TRUE))

# Paredes

datos %>%
  filter(
    rowSums(
      !is.na(select(., humedad_cocina, humedad_living, humedad_baño, humedad_dormitorios))
    ) > 0
  ) %>%
  
  View(datos_limpios)

# ___________________________________________
# VISUALIZAR DATOS

# Tabla de frecuencia según años de residencia en su vivienda.
datos_limpios %>% group_by(años_residencia) %>%
  summarize(cant = n())

# Tabla de frecuencia según cantidad de integrantes de la familia.
datos_limpios %>% group_by(cant_integrantes) %>%
  summarize(cant = n())

# Medidas resumen por grupo a partir del tipo de vivienda.
datos_limpios %>% group_by(tipo_tenencia) %>%
  summarize(cant_integrantes_media = mean(cant_integrantes),
            cant_integrantes_ds = sd(cant_integrantes))

# Tabla de intervalo de precios alquiler
datos_limpios %>% filter(!is.na(intervalo_alquiler)) %>%
  group_by(intervalo_alquiler) %>%
  summarize(cantidad = n())

# Cuantas persones tienen al menos una humedad en la casa dependiendo material de pared
group_by(material_paredes_exteriores) %>%
  summarise(cantidad_con_humedad = n()) %>%
  arrange(desc(cantidad_con_humedad)) %>%
  print()

# Medidas de posición.
min(datos$edad_jefe_hogar)
max(datos$edad_jefe_hogar)
