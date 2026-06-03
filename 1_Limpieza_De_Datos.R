library(tidyverse)
library(janitor) 
library(lubridate)

# Declaración de directorio
#setwd("C:/Users/Admin/Documents/UNAL/Estadística Descriptiva y Exploratoria/Proyecto/Limpieza de Datos/Archivos - Análisis/scripts")

# 1. Cargue de la base de datos original (desde la carpeta data/)
datos_raw <- read.csv("Sistema_de_Información_de_Eventos_de_Violencia_del_Conflicto_Armado_SIEVCAC_-_Víctimas_DF_Desaparición_Forzada_20260306.csv", 
                      fileEncoding = "UTF-8", 
                      na.strings = c("", "NA", "SIN INFORMACION", "SIN INFORMACIÓN", "0"))

# 2. Proceso de Limpieza Estructural
datos_limpios <- datos_raw %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  distinct() %>%
  
  # 2.1 Estandarización de Bogotá y San Andrés
  mutate(
    depto_tmp = toupper(trimws(as.character(departamento))),
    departamento = case_when(
      is.na(depto_tmp) ~ NA_character_,
      str_detect(depto_tmp, "BOGOT[AÁ]|SANTAFE") ~ "BOGOTA D.C.",
      str_detect(depto_tmp, "ANDR[EÉ]S|PROVIDENCIA|CATALINA") ~ "SAN ANDRES",
      TRUE ~ depto_tmp
    )
  ) %>%
  select(-depto_tmp) %>%
  
  # 2.2 Ajuste de variables temporales (Corrección de flotantes a enteros)
  mutate(
    ano_num = round(as.numeric(ano) * 1000),
    mes_num = as.numeric(mes),
    dia_num = as.numeric(dia)
  ) %>%
  
  # 2.3 Aplicación de ventana de observación (Post-2000 o Indeterminados)
  filter(ano_num >= 2000 | ano_num == 0 | is.na(ano_num)) %>%
  
  # 2.4 Construcción de la variable fecha
  mutate(
    fecha_evento = make_date(year = ano_num, month = mes_num, day = dia_num)
  ) %>%
  
  # 2.5 Creación de banderas (posterior uso en gráficas)
  mutate(
    flag_ano_desconocido = is.na(ano_num) | ano_num == 0,
    flag_geo_incompleta   = is.na(municipio) | is.na(departamento),
    flag_edad_desconocida = is.na(edad)
  ) %>%
  
  # 2.6 Conversión de variables categóricas a Factores
  mutate(across(
    .cols = c(id_caso, codigo_dane_de_municipio, municipio, departamento, 
              id_persona, sexo, etnia, ocupacion, calidad_de_la_victima_o_la_baja, 
              tipo_de_poblacion_vulnerable, militante_politico,
              fuerza_o_grupo_armado_organizado_al_que_pertenece_el_combatiente,
              descripcion_fuerza_o_grupo_armado_organizado_al_que_pertenece_el_combatiente,
              situacion_actual_de_la_victima, fuente_de_informacion_de_la_desaparicion,
              edad),
    .fns = as.factor
  )) %>%
  
  # 2.7 Remoción de columnas redundantes transformadas
  select(-ano, -mes, -dia)

# 3. Guardado en formato binario RDS para los siguientes módulos
saveRDS(datos_limpios, "datos_limpios.rds")
cat("✔ Proceso finalizado. Datos serializados en 'data/datos_limpios.rds'\n")
