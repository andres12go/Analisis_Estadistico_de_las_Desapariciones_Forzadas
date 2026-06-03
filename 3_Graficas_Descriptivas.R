library(tidyverse)
library(sf)        
library(viridis)   
library(stringi)   
library(tidyr)

datos <- readRDS("data/datos_limpios.rds")

# -----------------------------------------------------------------------------
# 1. Gráfico histórico temporal (histograma + polígono de frecuencias)
# -----------------------------------------------------------------------------
datos_para_grafica <- datos %>% filter(!is.na(fecha_evento))

grafico_ano <- ggplot(datos_para_grafica, aes(x = fecha_evento)) +
  geom_histogram(binwidth = 365, fill = "darkblue", color = "white", alpha = 0.7) +
  geom_line(stat = "bin", binwidth = 365, aes(y = after_stat(count)), color = "darkred", linewidth = 1) +
  geom_point(stat = "bin", binwidth = 365, aes(y = after_stat(count)), color = "darkred", size = 1.5) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  labs(title = "Evolución de Desapariciones Forzadas", subtitle = "Post año 2000", x = "Año del Evento", y = "Cantidad de Víctimas") +
  theme_minimal()

ggsave("01_Grafico_Evolucion_Por_Año.png", plot = grafico_ano, width = 14, height = 6, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 2. Mapa - Densidad de víctimas por departamento:
# -----------------------------------------------------------------------------
conteo_depto <- datos %>%
  filter(!is.na(departamento)) %>%
  mutate(
    depto_norm = toupper(trimws(as.character(departamento))),
    depto_norm = stri_trans_general(depto_norm, "Latin-ASCII")
  ) %>%
  group_by(depto_norm) %>%
  summarise(n_victimas = n(), .groups = "drop")

url_geojson <- "https://gist.githubusercontent.com/john-guerra/43c7656821069d00dcbc/raw/3aadedf47badbdac823b00dbe259f6bc6d9e1899/colombia.geo.json"
colombia_sf <- st_read(url_geojson, quiet = TRUE)

colombia_sf$depto_norm <- stri_trans_general(toupper(trimws(colombia_sf$NOMBRE_DPT)), "Latin-ASCII")
colombia_sf$depto_norm <- gsub("SANTAFE DE BOGOTA D\\.C", "BOGOTA D.C.", colombia_sf$depto_norm)
colombia_sf$depto_norm <- gsub("ARCHIPIELAGO DE SAN ANDRES.*", "SAN ANDRES", colombia_sf$depto_norm)

colombia_mapa <- merge(colombia_sf, conteo_depto, by = "depto_norm", all.x = TRUE)
colombia_mapa$n_victimas[is.na(colombia_mapa$n_victimas)] <- 0

mapa_calor <- ggplot(data = colombia_mapa) +
  geom_sf(aes(fill = n_victimas), color = "grey30", linewidth = 0.3) +
  scale_fill_viridis_c(option = "rocket", direction = -1, name = "Nº de Víctimas", labels = scales::comma_format()) +
  coord_sf(xlim = c(-82, -66), ylim = c(-5, 14), expand = FALSE) +
  labs(title = "Mapa de Calor: Desapariciones Forzadas en Colombia", subtitle = "Densidad por departamento") +
  theme_minimal()

ggsave("02_Mapa_Calor_Desapariciones.png", plot = mapa_calor, width = 10, height = 12, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 3. Diagrama circular (Militancia Política)
# -----------------------------------------------------------------------------
datos_militancia <- datos %>%
  filter(!is.na(militante_politico)) %>%
  group_by(militante_politico) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1))

paleta_sobria <- c(
  "#4A6274",   # Azul acero oscuro
  "#7A9EAF",   # Azul grisáceo
  "#A3C4BC",   # Verde salvia suave
  "#D4B483",   # Arena dorada
  "#C17767",   # Terracota sobria
  "#8B6F5E",   # Marrón cálido
  "#5C4A3E",   # Café oscuro
  "#6B7F5E",   # Verde musgo
  "#9E8C7A",   # Beige oscuro
  "gray"       # Gris
)

grafico_torta <- ggplot(datos_militancia, aes(x = "", y = n, fill = militante_politico)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = paleta_sobria, name = "Militancia") +
  geom_text(aes(label = ifelse(porcentaje >= 3, paste0(porcentaje, "%"), "")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  labs(title = "Distribución de Víctimas según Militancia Política") +
  theme_void()

ggsave("03_Grafico_Torta_Militancia.png", plot = grafico_torta, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 4. Gráfico de puntos (sobre Antioquia)
# -----------------------------------------------------------------------------

datos_antioquia <- datos %>%
  filter(toupper(trimws(as.character(departamento))) == "ANTIOQUIA") %>%
  filter(!is.na(latitud_longitud) & latitud_longitud != "") %>%
  
  # Eliminar la envoltura WKT: "POINT (-75.56 6.25)" → "-75.56 6.25"
  mutate(coord_limpia = gsub("POINT \\(|\\)", "", as.character(latitud_longitud))) %>%
  
  # Separar longitud y latitud (WKT: lng primero, lat después)
  separate(coord_limpia, into = c("lng", "lat"), sep = " ", convert = TRUE) %>%
  
  # Validar rango geográfico de Antioquia (bounding box aproximado)
  # Latitud:  ~5.4° a ~8.9°  |  Longitud: ~-77.1° a ~-73.9°
  filter(
    !is.na(lat) & !is.na(lng),
    lat > 5.4  & lat < 8.9,
    lng > -77.1 & lng < -73.9
  )

cat("\n--- Registros de Antioquia con coordenadas válidas:", nrow(datos_antioquia), "---\n")

# ---- 6.2 Convertir a objeto espacial sf (CRS WGS84 — EPSG:4326) ----
# st_as_sf() toma las columnas lng/lat y genera una geometría POINT
# coords = c("lng", "lat") → el orden es (x = longitud, y = latitud)
datos_antioquia_sf <- st_as_sf(
  datos_antioquia,
  coords = c("lng", "lat"),
  crs    = 4326,       # WGS84 — Sistema geodésico estándar GPS
  remove = FALSE       # Conservar las columnas numéricas originales
)

# ---- 6.3 Obtener la capa de MUNICIPIOS de Antioquia ----
# Fuente: GeoJSON municipal del Marco Geoestadístico Nacional (DANE 2018)
# Repositorio: github.com/caticoa3/colombia_mapa
# El archivo contiene TODOS los municipios de Colombia con código DANE.

url_municipios <- "https://raw.githubusercontent.com/caticoa3/colombia_mapa/master/co_2018_MGN_MPIO_POLITICO.geojson"

tryCatch({
  cat("\n⏳ Descargando GeoJSON de municipios de Colombia...\n")
  municipios_col <- st_read(url_municipios, quiet = TRUE)
  cat("✔ GeoJSON de municipios cargado correctamente.\n")
}, error = function(e) {
  # Fallback: si ya tienes el archivo descargado localmente
  if (file.exists("co_2018_MGN_MPIO_POLITICO.geojson")) {
    municipios_col <<- st_read("co_2018_MGN_MPIO_POLITICO.geojson", quiet = TRUE)
  } else {
    stop(paste0(
      "No se pudo descargar el GeoJSON de municipios. Error: ", e$message, "\n",
      "Descárgalo manualmente de: https://github.com/caticoa3/colombia_mapa\n",
      "y colócalo como 'co_2018_MGN_MPIO_POLITICO.geojson' en tu directorio de trabajo."
    ))
  }
})

# Filtrar SOLO los municipios de Antioquia
# El código DANE de Antioquia es "05" (campo DPTO_CCDGO)
antioquia_municipios <- municipios_col %>%
  filter(DPTO_CCDGO == "05")

# ---- MAPA ESTÁTICO con ggplot2 + geom_sf ----
# Renderizamos los puntos de desapariciones sobre el mapa político de Antioquia

mapa_antioquia_estatico <- ggplot() +
  
  # Capa 1: Polígonos municipales de Antioquia (mapa político)
  geom_sf(
    data      = antioquia_municipios,
    fill      = "#F0EDE8",           # Fondo pergamino suave
    color     = "#8C8C8C",           # Borde gris medio para límites municipales
    linewidth = 0.3
  ) +
  
  # Capa 2: Borde exterior del departamento (más grueso, para resaltar)
  geom_sf(
    data      = st_union(antioquia_municipios),  # Unir todos los municipios en un solo polígono
    fill      = NA,                              # Sin relleno (solo borde)
    color     = "#333333",                       # Borde oscuro
    linewidth = 0.9
  ) +
  
  # Capa 3: Puntos de desaparición forzada
  geom_sf(
    data     = datos_antioquia_sf,
    color    = "#B22222",            # Rojo oscuro (firebrick) — énfasis visual
    alpha    = 0.5,                  # Transparencia para mostrar densidad
    size     = 1.2,
    shape    = 16                    # Punto sólido
  ) +
  
  # Delimitar la vista al bounding box de Antioquia
  coord_sf(
    xlim   = c(-77.1, -73.9),
    ylim   = c(5.4, 8.9),
    expand = FALSE
  ) +
  
  # Títulos y anotaciones
  labs(
    title    = "Mapa de Puntos: Desaparición Forzada en Antioquia",
    subtitle = paste0(
      "Cada punto representa un registro georreferenciado de víctima (",
      format(nrow(datos_antioquia), big.mark = "."), " registros)"
    ),
    caption  = paste0(
      "Datos: Sistema de Información de Eventos de Violencia del Conflicto Armado (SIEVCAC)\n",
      "Geometría municipal: Marco Geoestadístico Nacional — DANE 2018\n",
      "Proyecto Estadística Descriptiva y Exploratoria — UNAL"
    ),
    x = "Longitud",
    y = "Latitud"
  ) +
  
  # Tema profesional
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "grey40"),
    plot.caption     = element_text(size = 8, hjust = 1, color = "grey50",
                                    margin = margin(t = 10)),
    panel.grid       = element_line(color = "grey90", linewidth = 0.2),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title       = element_text(size = 9, color = "grey50"),
    plot.margin      = margin(15, 15, 15, 15)
  )

# Guardar en alta resolución
ggsave("04_Mapa_Puntos_Antioquia.png", plot = mapa_antioquia_estatico, width= 10, height = 12, dpi = 300, bg = "white")

cat("Gráficos y mapas exportados con éxito a la carpeta 'output/'\n")
