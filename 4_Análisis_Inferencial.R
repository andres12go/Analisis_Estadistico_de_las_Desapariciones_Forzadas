library(tidyverse)
library(gtsummary)

datos <- readRDS("datos_limpios.rds")

# 1. Definición del Criterio de Riesgo Letal (Variable Binaria)
datos_preparados <- datos %>%
  mutate(
    Destino_Fatal = ifelse(situacion_actual_de_la_victima %in% c("APARECIÓ MUERTO", "MUERTO EN CAUTIVERIO"), 1, 0)
  ) %>%
  filter(!is.na(departamento))

# 2. Filtrado Dinámico de Separación Perfecta (Tu propuesta de control de errores)
departamentos_validos <- datos_preparados %>%
  group_by(departamento) %>%
  summarise(
    total_casos = n(),
    total_muertes = sum(Destino_Fatal),
    .groups = "drop"
  ) %>%
  filter(total_muertes > 0 & total_muertes < total_casos) %>%
  pull(departamento)

# Reducción espacial del modelo
datos_zonas <- datos_preparados %>%
  filter(departamento %in% departamentos_validos) %>%
  mutate(departamento = factor(departamento)) %>%
  # Establecer Bogotá como línea base analítica por estabilidad muestral
  mutate(departamento = relevel(departamento, ref = "BOGOTA D. C."))

# 3. Modelamiento mediante Máxima Verosimilitud (GLM Binomial)
modelo_zonas <- glm(
  Destino_Fatal ~ departamento, 
  data = datos_zonas, 
  family = binomial(link = "logit")
)

# 4. Construcción de Tabla de Regresión Académica Formal
tabla_peligrosidad <- modelo_zonas %>%
  tbl_regression(
    exponentiate = TRUE, 
    label = list(departamento ~ "Peligrosidad Relativa por Departamento (Línea Base: Bogotá D.C.)")
  ) %>%
  bold_p(t = 0.05) %>%   
  add_nevent()          

# Mostrar resultados analíticos (Ver tabla en sección viewer)
cat("---Modelo de regresión logística binaria geográfica ---\n")
tabla_peligrosidad