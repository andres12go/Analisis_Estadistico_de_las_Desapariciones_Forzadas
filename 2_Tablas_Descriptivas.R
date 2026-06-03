library(tidyverse)
library(knitr)

# Cargar datos serializados
datos <- readRDS("datos_limpios.rds")

# Función modular para la generación de bloques tabulares
generar_bloque <- function(data, columna_var, nombre_etiqueta) {
  data %>%
    count(Categoría = .data[[columna_var]]) %>%
    mutate(
      `Porcentaje (%)` = round((n / sum(n)) * 100, 2),
      Variable = nombre_etiqueta
    ) %>%
    select(Variable, Categoría, `Frecuencia (n)` = n, `Porcentaje (%)`) %>%
    arrange(desc(`Frecuencia (n)`))
}

# 1. Agrupación y colapso de Departamentos periféricos (Top 5 + Otros)
datos_g <- datos %>%
  mutate(Depto_Clean = fct_lump_n(factor(departamento), n = 5, other_level = "OTROS DEPARTAMENTOS")) %>%
  mutate(Depto_Clean = as.character(Depto_Clean))

# 2. Construcción unificada del perfil socio-demográfico
tabla_unida <- bind_rows(
  generar_bloque(datos_g, "sexo", "Sexo de la Víctima"),
  generar_bloque(datos_g, "edad", "Rango de Edad"),
  generar_bloque(datos_g, "etnia", "Etnia")
)

tabla_final <- tabla_unida %>%
  group_by(Variable) %>%
  mutate(Variable = ifelse(row_number() == 1, as.character(Variable), "")) %>%
  ungroup()

# Despliegue en consola R
cat("--- TABLA 1: PERFIL GENERAL DE LAS VÍCTIMAS ---\n")
print(tabla_final, n = Inf)

# 3. Tabla de contingencia cruzada (Sexo vs Edad) depurada
datos_filtrados <- datos %>%
  filter(!is.na(sexo) & !is.na(edad)) %>%
  filter(sexo != "SIN INFORMACION")

tabla_contingencia <- table(datos_filtrados$sexo, datos_filtrados$edad)

cat("\n--- TABLA 2: CONTINGENCIA (SEXO VS RANGO DE EDAD) ---\n")
print(tabla_contingencia)
