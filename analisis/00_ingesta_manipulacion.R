# ___________________________________________
# INSTALACIÓN DE PAQUETES E INCLUSIÓN DE LIBRERÍAS
# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googledrive")
# install.packages("readxl")
# install.packages("tidyverse")

library(googledrive)
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)


# ___________________________________________
# ADJUNTAR DATASET

# Descargo el archivo mediante su id de google drive
# El link de los archivos de drive tiene esta forma:
# https://docs.google.com/spreadsheets/d/1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy
# El id de esta hoja de cálculo es "1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"
googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), 
                            overwrite = T)

# Cargo el archivo como .xlsx.
datos = readxl::read_excel("Datos_LP.xlsx", col_names = TRUE, skip = 2)

# Renombrar columnas.
colnames(datos) = c(
  "id", "provincia", "barrio",
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
# FUNCIONES AUXILIARES
convertir_a_factor_ordenado <- function(var, niveles, etiquetas = NULL) {
  factor(var, levels = niveles, labels = etiquetas %||% niveles, ordered = TRUE)
}

limpiar_texto <- function(texto) {
  texto %>%
    str_to_lower() %>%
    str_trim() %>%
    str_squish()
}


# ___________________________________________
# LIMPIEZA DE DATOS

## 1. Limpieza de variable max_personas_dormitorio.
datos_limpios <- datos %>%
  mutate(
    hacinamiento_dormitorio = factor(
      case_when(
        max_personas_dormitorio <= 2 ~ "Sin hacinamiento (menos de 2 por dormitorio)",
        max_personas_dormitorio == 3 ~ "Moderado (3 por dormitorio)",
        max_personas_dormitorio >= 4 ~ "Crítico (más de 4 por dormitorio)"
      ),
      levels = c("Sin hacinamiento (menos de 2 por dormitorio)", "Moderado (3 por dormitorio)", "Crítico (más de 4 por dormitorio)")  # Orden de los niveles
    )
  ) %>%
  relocate(hacinamiento_dormitorio, .after = max_personas_dormitorio)


## 2. Limpieza de variables de vivienda.
datos_limpios <- datos_limpios %>%
    mutate(
      # Tipo de tenencia
      tipo_tenencia = case_when(
        tipo_tenencia == "Prestado" ~ "Prestado",
        tipo_tenencia == "Alquilado" ~ "Alquilado",
        tipo_tenencia == "Propio sin títulos" ~ "Propio sin títulos",
        tipo_tenencia == "Propio con algún comprobante de tenencia" ~ "Propio con comprobante",
        tipo_tenencia == "Ocupado/Tomado" ~ "Ocupado",
        tipo_tenencia == "Otro" ~ "Otro",
        TRUE ~ NA_character_
      ),
      
      # Costo de alquiler (convertir a numérico y categorizar)
      costo_alquiler_num = as.numeric(as.character(costo_alquiler)),
      costo_alquiler = cut(
        costo_alquiler_num,
        breaks = seq(0, 30000, by = 5000),
        labels = paste0(seq(0, 25000, by = 5000), "-", seq(5000, 30000, by = 5000)),
        include.lowest = TRUE,
        ordered_result = TRUE
      ),
      
      # Material de paredes (simplificar categorías)
      material_paredes_exteriores = fct_lump_min(material_paredes_exteriores, min = 10, other_level = "Otros materiales")
    )


## 3. Limpieza de variables de agua y saneamiento.
datos_limpios <- datos_limpios %>%
    mutate(
      # Presión del agua (asegurar orden correcto)
      presion_agua = convertir_a_factor_ordenado(
        presion_agua,
        niveles = c("Buena", "Débil", "Muy débil")
      ),
      
      # Agua caliente en baño (estandarizar categorías)
      agua_caliente_baño = case_when(
        is.na(agua_caliente_baño) ~ "No tiene",
        agua_caliente_baño == "No tengo agua caliente en el baño" ~ "No tiene",
        agua_caliente_baño == "Si, con un calefón eléctrico" ~ "Calefón eléctrico",
        agua_caliente_baño == "Si, con una ducha eléctrica" ~ "Ducha eléctrica",
        agua_caliente_baño == "Si, con un termotanque a gas" ~ "Termotanque a gas",
        agua_caliente_baño == "Si, con un termotanque eléctrico" ~ "Termotanque eléctrico"
      ),
      
      # Litros almacenados (convertir a numérico y categorizar)
      litros_intervalo = case_when(
        litros_almacenados == "Menos de 200 lts" ~ 100,
        litros_almacenados == "200 a 500 lts" ~ 350,
        litros_almacenados == "Más de 500 lts" ~ 600,
        TRUE ~ NA_real_
      ),
      litros_intervalo = cut(
        litros_intervalo,
        breaks = c(0, 200, 500, Inf),
        labels = c("[0, 200)", "[200, 500)", "[500, ∞)"),
        right = FALSE,
        ordered_result = TRUE
      )
    )


## 4. Limpieza de variables de energía.
datos_limpios = datos_limpios %>%
  mutate (
    energia_calefaccion_gas_natural = ifelse(!is.na(energia_calefaccion_gas_natural), 1, 0 ),
    energia_calefaccion_electricidad = ifelse(!is.na(energia_calefaccion_electricidad), 1, 0 ),
    energia_calefaccion_garrafa = ifelse(!is.na(energia_calefaccion_garrafa), 1, 0 ),
    energia_calefaccion_leña_carbon = ifelse(!is.na(energia_calefaccion_leña_carbon), 1, 0 ),
    energia_calefaccion_sin = ifelse(!is.na(energia_calefaccion_sin), 1, 0 ),
    energia_calefaccion_no_necesita = ifelse(!is.na(energia_calefaccion_no_necesita), 1, 0 )
  )


## 5. Limpieza de variables de humedad
datos_limpios <- datos_limpios %>%
    mutate(
      # Consolidar ubicaciones de humedad en una sola variable
      humedad_lugar = case_when(
        !is.na(humedad_cocina) ~ "Cocina",
        !is.na(humedad_living) ~ "Living",
        !is.na(humedad_baño) ~ "Baño",
        !is.na(humedad_dormitorios) ~ "Dormitorios",
        !is.na(humedad_sin) ~ "Ninguno",
        TRUE ~ NA_character_
      ),
      
      # Crear indicador binario de presencia de humedad
      tiene_humedad = ifelse(humedad_lugar != "Ninguno", 1, 0)
    )


## 6. Limpieza de variables de infraestructura
datos_limpios <- datos_limpios %>%
    mutate(
      # Calle asfaltada (estandarizar)
      calle_asfaltada = case_when(
        calle_asfaltqada == "Si" ~ "Sí",
        calle_asfaltqada == "No" ~ "No",
        TRUE ~ calle_asfaltqada
      ),
      
      # Riesgo de inundación (convertir a factor ordenado)
      riesgo_inundacion = convertir_a_factor_ordenado(
        riesgo_inundacion,
        niveles = c("Nunca", "Ocasionalmente", "Frecuentemente")
      )
    )
