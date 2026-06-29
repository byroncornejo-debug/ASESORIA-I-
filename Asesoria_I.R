
# ==============================================================================
# 0. LIBRERÍAS
# ==============================================================================

library(MASS)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ineq)
library(tidyr)
library(readr)
library(ggridges)

# ==============================================================================
# 1. CARGA DE DATOS
# ==============================================================================

chile_raw <- read_csv("C:/Users/byron/OneDrive/Desktop/asesoria1/Chile00-25.csv") %>% mutate(Pais = "Chile")
japon_raw <- read_csv("C:/Users/byron/OneDrive/Desktop/asesoria1/Japon00-25.csv") %>% mutate(Pais = "Japón")

trench <- st_read("C:/Users/byron/OneDrive/Desktop/asesoria1/PLATES_PlateBoundary_ArcGIS/trench.shp", quiet = TRUE)

# ==============================================================================
# 2. CÁLCULO DE FOSA MÁS CERCANA Y DISTANCIA (de fosas.R)
# ==============================================================================

trench_estudio <- trench %>%
  filter(
    geogdesc %in% c(
      "PERU-CHILE TRENCH",
      "CHILE TRENCH",
      "SOUTHERNMOST CHILE TRENCH",
      "KURIL TRENCH",
      "JAPAN TRENCH",
      "BONIN TRENCH",
      "NANKAI TROUGH",
      "RYUKU TRENCH"
    )
  ) %>%
  mutate(
    fosa = case_when(
      geogdesc %in% c(
        "PERU-CHILE TRENCH",
        "CHILE TRENCH",
        "SOUTHERNMOST CHILE TRENCH"
      ) ~ "PERU-CHILE TRENCH",
      
      TRUE ~ geogdesc
    )
  )

trench_m <- st_transform(trench_estudio, 3857)

# --- 2.2 Función reutilizable: calcula fosa más cercana y distancia (km) ---

calcular_fosa_cercana <- function(df, trench_m) {
  
  # Convertir los sismos a objeto sf
  sismos_sf <- st_as_sf(
    df,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE   # conservamos longitude/latitude como columnas normales
  )
  
  # Transformar a CRS métrico
  sismos_m <- st_transform(sismos_sf, 3857)
  
  # Matriz de distancias sismo -> cada fosa
  distancias <- st_distance(sismos_m, trench_m)
  
  # Fosa más cercana y distancia mínima
  indice_fosa <- apply(distancias, 1, which.min)
  dist_min    <- apply(distancias, 1, min)
  
  # Agregar variables al data frame original
  df$fosa         <- trench_m$fosa[indice_fosa]
  df$dist_fosa_km <- round(as.numeric(dist_min) / 1000, 2)
  
  df
}

# --- 2.3 Aplicar la función a cada país ---
Japon00_25 <- calcular_fosa_cercana(japon_raw, trench_m)
Chile00_25 <- calcular_fosa_cercana(chile_raw, trench_m)

# --- 2.4 Revisar resultados (igual que en fosas.R) ---
head(Japon00_25[, c("latitude", "longitude", "fosa", "dist_fosa_km")])
head(Chile00_25[, c("latitude", "longitude", "fosa", "dist_fosa_km")])

# ==============================================================================
# 3. UNIFICACIÓN Y PROCESAMIENTO DE DATOS (de PreasesoriaByronCornejo.R)
# ==============================================================================


datos <- bind_rows(Chile00_25, Japon00_25) %>%
  filter(
    (Pais == "Chile" & str_detect(place, "Chile") & !str_detect(place, "Argentina")) |
      (Pais == "Japón" & str_detect(place, "Japan")),
    !(magType %in% c("m", "ml"))
  ) %>%
  mutate(
    # Variable tricotomizada de profundidad
    depth_cat = case_when(
      depth < 70 ~ "Poco profunda",
      depth >= 70 & depth < 300 ~ "Intermedia",
      depth >= 300 & depth <= 700 ~ "Profunda",
      TRUE ~ NA_character_
    ),
    # Magnitudes homogéneas (transMw)
    transMw = case_when(
      magType %in% c("mw", "mwb", "mwc", "mwr", "mww") ~ mag,
      magType == "ms" & mag >= 3.0 & mag <= 6.1 ~ 0.67 * mag + 2.07,
      magType == "ms" & mag >= 6.2 & mag <= 8.2 ~ 0.99 * mag + 0.08,
      magType == "mb" & mag >= 3.5 & mag <= 6.7 ~ 0.85 * mag + 1.03,
      TRUE ~ NA_real_
    ),
    # Categorización de magnitud Mw_cat
    Mw_cat = factor(case_when(
      transMw >= 3 & transMw < 4 ~ "Minor",
      transMw >= 4 & transMw < 5 ~ "Light",
      transMw >= 5 & transMw < 6 ~ "Moderate",
      transMw >= 6 & transMw < 7 ~ "Strong",
      transMw >= 7 & transMw < 8 ~ "Major",
      transMw >= 8              ~ "Great",
      TRUE ~ NA_character_
    ), levels = c("Minor", "Light", "Moderate", "Strong", "Major", "Great")),
    year = year(time)
  )

# Verificación inicial de tipos de magnitud
with(datos, table(Pais, magType))

# ==============================================================================
# 4. CONFIGURACIÓN ESTÉTICA GLOBAL
# ==============================================================================

paleta_pais  <- c("Chile" = "#F8766D", "Japón" = "#619CFF")
paleta_mw    <- c("Minor" = "#D6EAF8", "Light" = "#AED6F1", "Moderate" = "#5DADE2", "Strong" = "#2874A6", "Major" = "#154360", "Great" = "red")
paleta_depth <- c("Poco profunda" = "#1f78b4", "Intermedia" = "#ff7f00", "Profunda" = "#e31a1c")

tema_base <- theme_minimal() +
  theme(
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title   = element_text(size = 16, face = "bold"),
    axis.text    = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 14)
  )

# ==============================================================================
# 5. MAPAS GEOREFERENCIADOS
# ==============================================================================

world <- ne_countries(scale = "medium", returnclass = "sf")

# Colores fijos de profundidad
paleta_depth <- c(
  "Poco profunda" = "#1f78b4",
  "Intermedia" = "#ff7f00",
  "Profunda" = "#e31a1c"
)

generar_mapa <- function(pais_nombre, xlims, ylims, r_size, forma_base, titulo) {
  
  paleta_formas <- c(
    "Moderate" = 21,
    "Strong"   = 22,
    "Major"    = 24,
    "Great"    = 23  
  )
  
  ggplot() +
    geom_sf(data = world, fill = "grey90", color = "grey40", linewidth = 0.2) +
    
    geom_point(data = datos %>% filter(Pais == pais_nombre, transMw >= 5),
               aes(x = longitude, y = latitude, size = transMw, fill = depth_cat, shape = Mw_cat),
               color = "black", stroke = 0.3, alpha = 0.8) +
    
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    
    scale_size_continuous(name = NULL, range = r_size, guide = "none") +
    scale_fill_manual(values = paleta_depth, name = "Profundidad") +
    
    scale_shape_manual(values = paleta_formas, name = "Categoría Mag", drop = TRUE) +
    
    labs(title = titulo, x = "Longitud", y = "Latitud") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 16, face = "bold"),
      axis.text  = element_text(size = 10),
      legend.title = element_text(size = 19, face = "bold"),
      legend.text  = element_text(size = 17),
      panel.grid.major = element_line(colour = "grey80", linewidth = 0.3)
    ) +
    
    guides(
      fill  = guide_legend(override.aes = list(shape = forma_base, size = 5)),
      shape = guide_legend(override.aes = list(size = 4, fill = "grey70"))
    )
}


generar_mapa("Chile", c(-80, -65), c(-55, -17), c(1, 5), forma_base = 22, "Sismicidad de Chile (Mw ≥ 5, 2000-2025)")

generar_mapa("Japón", c(120, 148), c(22, 50), c(1, 8), forma_base = 24, "Sismicidad de Japón (Mw ≥ 5, 2000-2025)")

# ==============================================================================
# 6. ESTADÍSTICAS DESCRIPTIVAS Y TABLAS DE CONTINGENCIA
# ==============================================================================

# Resumen numérico general
resumen_cuant <- function(data) {
  data %>%
    select(where(is.numeric) & c(depth, transMw)) %>%
    summarise(across(everything(), list(
      n = ~sum(!is.na(.)), Media = ~mean(., na.rm = TRUE), DE = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE), Q25 = ~quantile(., 0.25, na.rm = TRUE),
      Mediana = ~median(., na.rm = TRUE), Q75 = ~quantile(., 0.75, na.rm = TRUE), Max = ~max(., na.rm = TRUE)
    ))) %>%
    pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_")
}

resumen_cuant(datos %>% filter(Pais == "Chile"))
resumen_cuant(datos %>% filter(Pais == "Japón"))

# Generación automática de tablas de frecuencias relativas
cats <- c("status", "type", "locationSource", "magSource", "depth_cat", "Mw_cat")
tablas <- lapply(cats, function(v) {
  datos %>%
    group_by(Pais, across(all_of(v))) %>%
    summarise(Frecuencia = n(), .groups = "drop_last") %>%
    mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2))
})
names(tablas) <- cats

# Ejemplo para revisar una tabla específica:
tablas$depth_cat
tablas$Mw_cat

# Resumen detallado de profundidad según categoría de profundidad
datos %>%
  group_by(Pais, depth_cat) %>%
  summarise(n = n(), Media = mean(depth, na.rm=T), DE = sd(depth, na.rm=T), Min = min(depth, na.rm=T),
            Q1 = quantile(depth, 0.25, na.rm=T), Mediana = median(depth, na.rm=T),
            Q3 = quantile(depth, 0.75, na.rm=T), Max = max(depth, na.rm=T), .groups = "drop")

# ==============================================================================
# 7. ANÁLISIS GRÁFICO (Cajas, Violines y Densidades)
# ==============================================================================

# Función para gráficos de Caja y Violín básicos por país
graficar_box_violin <- function(y_var, ylab, titulo, ylims = NULL, t_size = 22) {
  p <- ggplot(datos, aes(x = Pais, y = .data[[y_var]], fill = Pais)) +
    geom_violin(alpha = 0.35, colour = NA, trim = FALSE) +
    geom_boxplot(width = 0.18, colour = "black", alpha = 0.8, outlier.shape = 21, outlier.size = 2) +
    stat_summary(fun = median, geom = "point", shape = 23, size = 0, fill = "yellow") +
    scale_fill_manual(values = paleta_pais) +
    labs(title = titulo, x = "", y = ylab) +
    tema_base + theme(legend.position = "none", panel.grid.major.x = element_blank(), plot.title = element_text(size = t_size, face = "bold", hjust = 0.5))
  if(!is.null(ylims)) p <- p + coord_cartesian(ylim = ylims)
  print(p)
}

graficar_box_violin("transMw", expression(M[w]), "Comparación de la Magnitud de Momento")
graficar_box_violin("depth", "Profundidad (km)", "Comparación de la Profundidad de los Sismos", c(0, 350), t_size = 15)

# Distribución de la profundidad según categoría y país
datos %>%
  mutate(Grupo = factor(paste(depth_cat, Pais), levels = c("Poco profunda Chile", "Poco profunda Japón", "Intermedia Chile", "Intermedia Japón", "Profunda Chile", "Profunda Japón"))) %>%
  ggplot(aes(x = Grupo, y = depth, fill = Pais)) +
  geom_violin(alpha = 0.3, colour = NA, trim = FALSE) +
  geom_boxplot(width = 0.18, colour = "black", alpha = 0.8, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(values = paleta_pais) +
  scale_y_continuous(breaks = seq(0, 700, 100)) + coord_cartesian(ylim = c(0, 700)) +
  labs(title = "Distribución de la profundidad según categoría y país", x = "", y = "Profundidad (km)", fill = "País") +
  tema_base + theme(axis.text.x = element_text(size = 12, angle = 20, hjust = 1))

# Función para distribuciones de densidad
graficar_densidad <- function(x_var, xlab, titulo, facet_var = NULL, filtrar_mw = FALSE) {
  df_plot <- if(filtrar_mw) datos %>% filter(Mw_cat %in% c("Moderate", "Strong", "Major")) else datos
  p <- ggplot(df_plot, aes(x = .data[[x_var]], fill = Pais, colour = Pais)) +
    geom_density(alpha = 0.35, linewidth = 1.2, adjust = if(x_var == "transMw") 1.2 else 1) +
    scale_fill_manual(values = paleta_pais) + scale_colour_manual(values = paleta_pais) +
    labs(title = titulo, x = xlab, y = "Densidad", fill = "País", colour = "País") +
    tema_base + theme(plot.title = element_text(size = if(is.null(facet_var)) 20 else 15, face = "bold", hjust = 0.5))
  
  if(!is.null(facet_var)) {
    f_levels <- if(facet_var == "Mw_cat") c("Moderate", "Strong", "Major") else unique(datos[[facet_var]])
    p <- p + facet_wrap(vars(factor(.data[[facet_var]], levels = f_levels)), nrow = 1, scales = "free_y") +
      theme(strip.text = element_text(size = 16, face = "bold"))
  }
  print(p)
}

graficar_densidad("transMw", expression(M[w]), "Distribución de la Magnitud de Momento")
graficar_densidad("transMw", expression(M[w]), "Distribución de la Magnitud de Momento según Profundidad", "depth_cat")
graficar_densidad("depth", "Profundidad (km)", "Distribución de la Profundidad de los Sismos")
graficar_densidad("depth", "Profundidad (km)", "Distribución de la Profundidad según Magnitud", "Mw_cat", filtrar_mw = TRUE)

# ==============================================================================
# 8. ANÁLISIS DE FRECUENCIAS Y PROPORCIONES (Gráficos de Barras)
# ==============================================================================

# Frecuencia anual de sismos
ggplot(datos, aes(x = year, fill = Mw_cat)) +
  geom_bar() + facet_wrap(~Pais, ncol = 1) +
  labs(title = "Frecuencia anual según categoría de magnitud", x = "Año", y = "Número de sismos", fill = "Categoría") +
  theme_minimal()

# Proporción anual, por profundidad y por magnitud
graficar_barras_prop <- function(x_var, fill_var, paleta, titulo, xlab, ylab) {
  ggplot(datos, aes(x = .data[[x_var]], fill = .data[[fill_var]])) +
    geom_bar(position = "fill") + facet_wrap(~Pais, ncol = if(x_var == "year") 1 else NULL) +
    scale_fill_manual(values = paleta) +
    labs(title = titulo, x = xlab, y = ylab, fill = fill_var) +
    tema_base + theme(axis.text = element_text(size = 18), strip.text = element_text(size = 18, face = "bold"))
}

graficar_barras_prop("year", "Mw_cat", paleta_mw, "Proporción anual según categoría de magnitud", "Año", "Proporción")
graficar_barras_prop("depth_cat", "Mw_cat", paleta_mw, "Magnitud según categoría de profundidad", "Profundidad", "Proporción")
graficar_barras_prop("Mw_cat", "depth_cat", c("Poco profunda"="#9ECAE1", "Intermedia"="#4292C6", "Profunda"="#084594"),
                     "Distribución de la profundidad según magnitud", "Categoría de magnitud", "Proporción")

# ==============================================================================
# 9. ANÁLISIS DE SERIES TEMPORALES
# ==============================================================================

# Preparar las series mensuales rellenando vacíos con cero sismos
preparar_ts <- function(df) {
  df %>%
    mutate(mes = floor_date(time, "month")) %>%
    count(Pais, mes) %>%
    group_by(Pais) %>%
    complete(mes = seq(min(mes), max(mes), by = "month"), fill = list(n = 0)) %>%
    ungroup()
}

datos_ts    <- preparar_ts(datos)
datos_ts_m6 <- preparar_ts(datos %>% filter(transMw >= 6))

# Gráfico de líneas temporales mensuales
graficar_lineas_ts <- function(df, titulo) {
  ggplot(df, aes(x = mes, y = n, colour = Pais)) +
    geom_line(linewidth = if(stringr::str_detect(titulo, "Mw ≥ 6")) 1.2 else 1.1) +
    scale_colour_manual(values = paleta_pais) +
    labs(title = titulo, x = "Tiempo", y = "Número de sismos", colour = "País") +
    tema_base + theme(axis.text = element_text(size = 18), strip.text = element_text(size = 18, face = "bold"))
}

graficar_lineas_ts(datos_ts, "Frecuencia mensual de sismos")
graficar_lineas_ts(datos_ts_m6, "Frecuencia mensual de sismos Mw ≥ 6")

# Gráfico de medias anuales agregadas
graficar_medias_anuales <- function(df, titulo) {
  df %>%
    mutate(Año = year(mes)) %>%
    group_by(Pais, Año) %>%
    summarise(Nivel = mean(n), .groups = "drop") %>%
    ggplot(aes(x = Año, y = Nivel, colour = Pais)) +
    geom_line(linewidth = 1.3) + geom_point(size = 2) +
    scale_colour_manual(values = paleta_pais) +
    labs(title = titulo, x = "Año", y = "Frecuencia mensual promedio", colour = "País") +
    tema_base + theme(axis.text = element_text(size = 18), strip.text = element_text(size = 18, face = "bold"))
}

graficar_medias_anuales(datos_ts, "Media de las frecuencias mensuales por año")
graficar_medias_anuales(datos_ts_m6, "Media anual de las frecuencias mensuales Mw ≥ 6")

# ==============================================================================
# 10. CURVAS DE LORENZ
# ==============================================================================

Lc_chile <- Lc((datos_ts %>% filter(Pais == "Chile"))$n)
Lc_japon <- Lc((datos_ts %>% filter(Pais == "Japón"))$n)

plot(Lc_chile, col = "#F8766D", lwd = 3, main = "Curvas de Lorenz de las frecuencias mensuales",
     xlab = "Proporción acumulada de meses", ylab = "Proporción acumulada de sismos",
     cex.main = 1.6, cex.lab = 1.4, cex.axis = 1.6)
lines(Lc_japon$p, Lc_japon$L, col = "#619CFF", lwd = 3)
abline(0, 1, lty = 2, lwd = 2)
legend("topleft", legend = c("Chile", "Japón"), col = paleta_pais, lwd = 3, bty = "n", cex = 1.3)

# ==============================================================================
# 11. TIEMPOS INTER-EVENTOS
# ==============================================================================

# Bloque 1: Filtrar y calcular la diferencia de tiempo
inter_eventos <- datos %>%
  filter(Mw_cat %in% c("Strong", "Major", "Great")) %>%
  arrange(Pais, Mw_cat, time) %>%
  group_by(Pais, Mw_cat) %>%
  mutate(Tiempo = as.numeric(difftime(time, lag(time), units = "days"))) %>%
  filter(!is.na(Tiempo)) %>%
  ungroup()

# Bloque 2: Generar la tabla de estadísticas descriptivas
tabla_inter <- inter_eventos %>%
  group_by(Pais, Mw_cat) %>%
  summarise(
    n       = n(),
    Media   = mean(Tiempo),
    DE      = sd(Tiempo),
    Min     = min(Tiempo),
    Q1      = quantile(Tiempo, .25),
    Mediana = median(Tiempo),
    Q3      = quantile(Tiempo, .75),
    Max     = max(Tiempo),
    .groups = "drop"
  )

# Desplegar la tabla en consola
print(tabla_inter)

# ==============================================================================
# 12. ANÁLISIS PRELIMINAR POR SISTEMA DE SUBDUCCIÓN (SECCIÓN 5.5 DEL INFORME)
# ==============================================================================

# ------------------------------------------------------------------------------
# 12.1 Etiqueta de visualización para las fosas (corrección del typo "RYUKU")
# ------------------------------------------------------------------------------


datos <- datos %>%
  mutate(
    fosa_label = case_when(
      fosa == "RYUKU TRENCH" ~ "RYUKYU TRENCH",
      TRUE ~ fosa
    )
  )

# ------------------------------------------------------------------------------
# 12.2 Estadísticas descriptivas por fosa: transMw y depth
# ------------------------------------------------------------------------------
# Mismo formato que el Cuadro 1 del informe (n, Media, DE, Min, Q25, Mediana,
# Q75, Max), pero agrupado por Pais + fosa_label en lugar de solo por Pais.

resumen_por_fosa <- function(data, variable, etiqueta) {
  data %>%
    filter(!is.na(fosa_label), !is.na(.data[[variable]])) %>%
    group_by(Pais, fosa_label) %>%
    summarise(
      Variable = etiqueta,
      n       = n(),
      Media   = round(mean(.data[[variable]]), 2),
      DE      = round(sd(.data[[variable]]), 2),
      Min     = round(min(.data[[variable]]), 2),
      Q25     = round(quantile(.data[[variable]], 0.25), 2),
      Mediana = round(median(.data[[variable]]), 2),
      Q75     = round(quantile(.data[[variable]], 0.75), 2),
      Max     = round(max(.data[[variable]]), 2),
      .groups = "drop"
    ) %>%
    relocate(Variable, .after = fosa_label)
}

desc_transMw_fosa <- resumen_por_fosa(datos, "transMw", "transMw")
desc_depth_fosa   <- resumen_por_fosa(datos, "depth",   "depth")

tabla_descriptiva_fosa <- bind_rows(desc_transMw_fosa, desc_depth_fosa) %>%
  arrange(Pais, fosa_label, Variable)

print(tabla_descriptiva_fosa)

# ------------------------------------------------------------------------------
# 12.3 Tablas de frecuencia por fosa: status, depth_cat, Mw_cat
# ------------------------------------------------------------------------------
# Mismo formato que el Cuadro 2 del informe (Frecuencia y Porcentaje), pero
# calculado dentro de cada combinación Pais + fosa.

tabla_categorica_por_fosa <- function(data, variable) {
  data %>%
    filter(!is.na(fosa_label), !is.na(.data[[variable]])) %>%
    group_by(Pais, fosa_label, across(all_of(variable))) %>%
    summarise(Frecuencia = n(), .groups = "drop_last") %>%
    mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
    ungroup()
}

tabla_status_fosa    <- tabla_categorica_por_fosa(datos, "status")
tabla_depth_cat_fosa <- tabla_categorica_por_fosa(datos, "depth_cat")
tabla_mw_cat_fosa    <- tabla_categorica_por_fosa(datos, "Mw_cat")

print(tabla_status_fosa)
print(tabla_depth_cat_fosa)
print(tabla_mw_cat_fosa)

# ------------------------------------------------------------------------------
# 12.4 Ridgeline plots: distribución de magnitud y profundidad por fosa
# ------------------------------------------------------------------------------

# --- Ridgeline 1: Distribución de la magnitud de momento (transMw) por fosa ---
ggplot(datos %>% filter(!is.na(fosa_label)),
       aes(x = transMw, y = fosa_label, fill = fosa_label)) +
  geom_density_ridges(scale = 1.5, alpha = 0.85, colour = "black", linewidth = 0.3) +
  scale_fill_viridis_d(option = "rocket", direction = -1, guide = "none") +
  labs(
    title = "Distribución de la magnitud de momento por fosa",
    x = expression(M[w]),
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# --- Ridgeline 2: Distribución de la profundidad (depth) por fosa ---
# Se agregan líneas verticales marcadas en 70 km y 300 km, que son los
# cortes usados para construir la variable depth_cat (Poco profunda: <70,
# Intermedia: 70-300, Profunda: >300), de modo que se vea directamente en
# el gráfico dónde cae cada sismo respecto a esa clasificación.
ggplot(datos %>% filter(!is.na(fosa_label)),
       aes(x = depth, y = fosa_label, fill = fosa_label)) +
  geom_density_ridges(scale = 1.5, alpha = 0.85, colour = "black", linewidth = 0.3) +
  geom_vline(xintercept = c(70, 300), linetype = "solid", colour = "#8B0000", linewidth = 1.2) +
  annotate("text", x = 70,  y = Inf, label = "70 km",  vjust = -0.6, hjust = -0.15,
           size = 5, fontface = "bold", colour = "#8B0000") +
  annotate("text", x = 300, y = Inf, label = "300 km", vjust = -0.6, hjust = -0.15,
           size = 5, fontface = "bold", colour = "#8B0000") +
  coord_cartesian(clip = "off") +
  scale_fill_viridis_d(option = "rocket", direction = -1, guide = "none") +
  labs(
    title = "Distribución de la profundidad por fosa",
    x = "Profundidad (km)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 30, r = 15, b = 10, l = 10)
  )

# ==============================================================================
# 13. ASOCIACIÓN ENTRE depth_cat Y Mw_cat, POR PAÍS
# ==============================================================================
# A diferencia del intento por fosa (descartado: en Chile fosa y país están
# casi confundidos, y no hay un mecanismo sismológico claro que justifique
# desagregar la asociación magnitud-profundidad fosa por fosa), este bloque
# evalúa la asociación a nivel de país, donde el contraste es sustantivo:
# Nazca-Sudamericana (Chile) vs. el sistema multi-placa de Japón.

# ------------------------------------------------------------------------------
# 13.1 Tabla de contingencia: depth_cat x Mw_cat, por país
# ------------------------------------------------------------------------------

# --- Tabla larga: una fila por cada combinación Pais-depth_cat-Mw_cat ---
tabla_contingencia_pais_larga <- datos %>%
  filter(!is.na(depth_cat), !is.na(Mw_cat)) %>%
  count(Pais, depth_cat, Mw_cat, name = "Frecuencia")

print(tabla_contingencia_pais_larga)

# --- Tablas de contingencia (formato ancho), una por país ---
# depth_cat en filas, Mw_cat en columnas.
generar_tabla_contingencia_pais <- function(data, pais_nombre) {
  data %>%
    filter(Pais == pais_nombre, !is.na(depth_cat), !is.na(Mw_cat)) %>%
    count(depth_cat, Mw_cat) %>%
    pivot_wider(names_from = Mw_cat, values_from = n, values_fill = 0)
}

tabla_contingencia_chile <- generar_tabla_contingencia_pais(datos, "Chile")
tabla_contingencia_japon <- generar_tabla_contingencia_pais(datos, "Japón")

cat("\n=== Tabla de contingencia: Chile ===\n")
print(tabla_contingencia_chile)

cat("\n=== Tabla de contingencia: Japón ===\n")
print(tabla_contingencia_japon)

# ------------------------------------------------------------------------------
# 13.2 Correlación de Spearman entre Mw_cat y depth_cat, por país
# ------------------------------------------------------------------------------

datos <- datos %>%
  mutate(
    depth_cat_rango = case_when(
      depth_cat == "Poco profunda" ~ 1,
      depth_cat == "Intermedia"    ~ 2,
      depth_cat == "Profunda"      ~ 3,
      TRUE ~ NA_real_
    ),
    Mw_cat_rango = case_when(
      Mw_cat == "Moderate" ~ 1,
      Mw_cat == "Strong"   ~ 2,
      Mw_cat == "Major"    ~ 3,
      Mw_cat == "Great"    ~ 4,
      TRUE ~ NA_real_
    )
  )

spearman_seguro <- function(x, y) {
  ok <- complete.cases(x, y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3 || sd(x) == 0 || sd(y) == 0) {
    return(c(rho = NA_real_, p_value = NA_real_))
  }
  test <- suppressWarnings(cor.test(x, y, method = "spearman"))
  c(rho = round(unname(test$estimate), 3), p_value = signif(test$p.value, 4))
}

correlacion_mwcat_depthcat_por_pais <- datos %>%
  filter(!is.na(Pais)) %>%
  group_by(Pais) %>%
  group_modify(~ {
    res <- spearman_seguro(.x$Mw_cat_rango, .x$depth_cat_rango)
    data.frame(
      n                     = nrow(.x),
      n_cat_depth_presentes = length(unique(.x$depth_cat_rango[!is.na(.x$depth_cat_rango)])),
      n_cat_mw_presentes    = length(unique(.x$Mw_cat_rango[!is.na(.x$Mw_cat_rango)])),
      rho                   = res["rho"],
      p_value               = res["p_value"]
    )
  }) %>%
  ungroup()

print(correlacion_mwcat_depthcat_por_pais)

# ==============================================================================
# FIN BLOQUE 13
# ==============================================================================

# ==============================================================================
# 14. ASOCIACIÓN ENTRE dist_fosa_km Y depth, POR PAÍS
# ==============================================================================
# 14.1 Gráfico de dispersión: dist_fosa_km vs depth, por país
# ------------------------------------------------------------------------------

datos_dist_depth <- datos %>% filter(!is.na(dist_fosa_km), !is.na(depth))

ggplot(datos_dist_depth, aes(x = dist_fosa_km, y = depth)) +
  geom_point(alpha = 0.35, size = 1.3, colour = "grey30") +
  geom_smooth(aes(colour = "Pearson"),  method = "lm",    se = FALSE, linewidth = 1.3, linetype = "dashed") +
  geom_smooth(aes(colour = "Spearman"),   method = "loess", se = FALSE, linewidth = 1.3) +
  scale_colour_manual(name = NULL, values = c("Pearson" = "#2874A6", "Spearman" = "#E74C3C")) +
  facet_wrap(~Pais, scales = "free") +
  labs(
    title = "Distancia a la fosa vs profundidad, por país",
    x = "Distancia a la fosa (km)",
    y = "Profundidad (km)"
  ) +
  tema_base +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )

# ------------------------------------------------------------------------------
# 14.2 Coeficientes de correlación: Pearson y Spearman, por país
# ------------------------------------------------------------------------------

calcular_correlaciones_dist_depth <- function(data, pais_nombre) {
  sub <- data %>% filter(Pais == pais_nombre)
  data.frame(
    Pais     = pais_nombre,
    n        = nrow(sub),
    Pearson  = round(cor(sub$dist_fosa_km, sub$depth, method = "pearson"), 3),
    Spearman = round(cor(sub$dist_fosa_km, sub$depth, method = "spearman"), 3)
  )
}

correlacion_dist_depth_por_pais <- bind_rows(
  calcular_correlaciones_dist_depth(datos_dist_depth, "Chile"),
  calcular_correlaciones_dist_depth(datos_dist_depth, "Japón")
)

print(correlacion_dist_depth_por_pais)

# ==============================================================================
# FIN BLOQUE 14
# ==============================================================================


# ==============================================================================
# 15. ASOCIACIÓN ENTRE transMw Y depth (VARIABLES CONTINUAS), POR PAÍS
# ==============================================================================

# ------------------------------------------------------------------------------
# 15.1 Gráfico de dispersión: transMw vs depth, por país
# ------------------------------------------------------------------------------

datos_mw_depth <- datos %>% filter(!is.na(transMw), !is.na(depth))

ggplot(datos_mw_depth, aes(x = depth, y = transMw)) +
  geom_point(alpha = 0.35, size = 1.3, colour = "grey30") +
  geom_smooth(aes(colour = "Pearson"),  method = "lm",    se = FALSE, linewidth = 1.3, linetype = "dashed") +
  geom_smooth(aes(colour = "Spearman"), method = "loess", se = FALSE, linewidth = 1.3) +
  scale_colour_manual(name = NULL, values = c("Pearson" = "#2874A6", "Spearman" = "#E74C3C")) +
  facet_wrap(~Pais, scales = "free") +
  labs(
    title = "Profundidad vs magnitud de momento, por país",
    x = "Profundidad (km)",
    y = expression(M[w])
  ) +
  tema_base +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )

# ------------------------------------------------------------------------------
# 15.2 Coeficientes de correlación: Pearson y Spearman, por país
# ------------------------------------------------------------------------------

calcular_correlaciones_mw_depth <- function(data, pais_nombre) {
  sub <- data %>% filter(Pais == pais_nombre)
  data.frame(
    Pais     = pais_nombre,
    n        = nrow(sub),
    Pearson  = round(cor(sub$depth, sub$transMw, method = "pearson"), 3),
    Spearman = round(cor(sub$depth, sub$transMw, method = "spearman"), 3)
  )
}

correlacion_mw_depth_por_pais <- bind_rows(
  calcular_correlaciones_mw_depth(datos_mw_depth, "Chile"),
  calcular_correlaciones_mw_depth(datos_mw_depth, "Japón")
)

print(correlacion_mw_depth_por_pais)

# ==============================================================================
# FIN BLOQUE 15
# ==============================================================================
