# ---------------------------------------------------------------
# Pipeline reproducible de análisis estadístico para `test_data`
# Incendios forestales (CAR / SINCHI) — R script
# ---------------------------------------------------------------
# Objetivo: implementar, paso a paso y auditable, el flujo solicitado:
# 1) EDA inicial; 2) Missing; 3) Outliers; 4) Feature engineering;
# 5) PCA (solo continuas) + Clustering (dos rutas); 6) Reporte de decisiones
# ---------------------------------------------------------------
# NOTAS IMPORTANTES
# - El script no usa `area_afectada` para crear flags (evitar leakage)
# - KNN (k=5) para numéricas; moda para categóricas
# - Winsorización 3×IQR se aplica SOLO si reduce |skew| >=20% y cambia mediana <5%
# - Colinealidad: umbral |r| > 0.85 (greedy, elimina la variable más redundante)
# - PCA: solo continuas estandarizadas; Clustering: Ward.D2 (continuas) y K-means (PCs)
# - Seed fija para reproducibilidad
# ---------------------------------------------------------------

# ================ 0) Librerías y parámetros =====================
required_pkgs <- c(
  "tidyverse", "lubridate", "janitor", "VIM", "moments",
  "cluster", "clusterCrit", "FactoMineR", "factoextra",
  "ggcorrplot", "patchwork"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_pkgs, installed)
if (length(missing) > 0) {
  message("Instalando paquetes faltantes: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(VIM)
  library(moments)
  library(cluster)
  library(clusterCrit)
  library(FactoMineR)
  library(factoextra)
  library(ggcorrplot)
  library(patchwork)
})

set.seed(42)

# Parámetros editables (ajusta a criterio)
PAR <- list(
  PATH                = "data/final/test_data_modelos.csv",   # <- sustituye si tu ruta es distinta
  KNN_K               = 5,
  OUTLIER_RULE        = 100,                # 3×IQR para capado/winsor
  CORR_THRESHOLD      = 0.99,               # |r| > 0.85 => eliminar 1 de cada par
  VAR_THRESHOLD_ZVAR  = 0.10,               # elimina var(z) <= 0.10 (si aplica)
  FLAG_PERC           = 0.25,               # percentil 25 para proximidades
  PCA_VAR_TARGET      = 0.61,               # % acumulado para truncar PCs
  K_RANGE             = 3:8,                # rango de k para clustering
  RUN_FAMD            = TRUE,              # TRUE si quieres comparar con FAMD
  RUN_CLUSTGEO        = FALSE               # TRUE si ponderas espacio (requiere clustGeo)
)

# ================ 1) Carga y tipificación =======================
stopifnot(file.exists(PAR$PATH))
raw <- readr::read_csv(PAR$PATH, show_col_types = FALSE) |> clean_names()

#raw <- test_data

# Coerción mínima garantizada
expected_cols <- c(
  "latitud", "longitud", "fecha",
  "dist_urbano", "dist_areas_protegidas", "dist_drenaje_doble",
  "dist_vias_primarias", "dist_embalses", "dist_lineas_electricas",
  "elevacion", "orientacion", "pendiente", "sombreado", "cobertura",
  "poblacion", "educacion", "pobreza",
  "ndvi_evt", "ndvi_prev", "prcp_evt", "prcp_prev",
  "tmax_evt", "tmax_prev", "tmin_evt", "tmin_prev", "rh_evt", "rh_prev"
)
missing_cols <- setdiff(expected_cols, names(raw))
if (length(missing_cols) > 0) stop("Faltan columnas en el CSV: ", paste(missing_cols, collapse=", "))

df <- raw |>
  mutate(
    fecha_inicio = as.Date(fecha),
    cobertura    = as.integer(cobertura)#,    municipio    = as.character(municipio)
  )

cat("\nDimensiones:", nrow(df), "x", ncol(df), "\n")

# ================ 2) EDA inicial ================================
# Tabla tipos y %NA
na_pct <- function(x) mean(is.na(x))*100
eda_types_before <- tibble(
  variable = names(df),
  type     = vapply(df, function(x) class(x)[1], character(1)),
  na_pct   = vapply(df, na_pct, numeric(1))
) |>
  arrange(desc(na_pct))

print(eda_types_before, n = Inf)

# Estacionalidad
df <- df |>
  mutate(
    anio = year(fecha_inicio),
    mes  = month(fecha_inicio),
    fin_semana = as.integer(wday(fecha_inicio) %in% c(1,7))
  )

mes_counts  <- count(df, mes, name = "n_eventos")
anio_counts <- count(df, anio, name = "n_eventos")

cat("\nEventos por mes (conteo):\n"); print(mes_counts)
cat("\nEventos por año (conteo):\n"); print(anio_counts)

# Correlaciones entre numéricas (Pearson y Spearman)
num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
num_df   <- df |> select(all_of(num_vars))

cor_top_abs <- function(mat, top = 10) {
  m <- as_tibble(as.table(mat), .name_repair = "minimal")
  names(m) <- c("var1", "var2", "r")
  m |>
    filter(var1 < var2) |>
    mutate(abs_r = abs(r)) |>
    arrange(desc(abs_r)) |>
    slice_head(n = top)
}

corP <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")
corS <- cor(num_df, use = "pairwise.complete.obs", method = "spearman")

cat("\nTop 10 correlaciones |r| (Pearson):\n"); print(cor_top_abs(corP, 10))
cat("\nTop 10 correlaciones |rho| (Spearman):\n"); print(cor_top_abs(corS, 10))

# ================ 3) Missing values =============================
# Umbral 25% y registro de variables eliminadas
vars_drop_na <- eda_types_before |>
  filter(na_pct > 25) |>
  pull(variable)

if (length(vars_drop_na)) {
  message("Eliminando por >25% NA: ", paste(vars_drop_na, collapse=", "))
}

df_miss <- df |>
  select(-any_of(vars_drop_na))

# Imputación categórica (moda) para 'cobertura' si aplica
mode_impute <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(x)
  val <- names(which.max(table(ux)))
  x[is.na(x)] <- as.numeric(val)
  x
}

if ("cobertura" %in% names(df_miss)) {
  df_miss <- df_miss |>
    mutate(cobertura = mode_impute(cobertura))
}

# Imputación numérica KNN k=5 (estandarizando internamente)
num_na_vars <- df_miss |>
  select(where(is.numeric)) |>
  select(where(~ any(is.na(.x)))) |>
  names()

if (length(num_na_vars) > 0) {
  num_dat <- df_miss |> select(all_of(num_na_vars)) |> as.data.frame()
  # Escalado robusto para KNN
  mu  <- vapply(num_dat, function(x) mean(x, na.rm=TRUE), numeric(1))
  sdv <- vapply(num_dat, function(x) sd(x, na.rm=TRUE), numeric(1))
  sdv[sdv == 0 | is.na(sdv)] <- 1
  num_z  <- as.data.frame(scale(num_dat, center = mu, scale = sdv))
  imp_z  <- VIM::kNN(num_z, k = PAR$KNN_K, imp_var = FALSE)
  imp    <- sweep(as.data.frame(imp_z), 2, sdv, `*`) |> sweep(2, mu, `+`)
  df_miss[num_na_vars] <- imp
}

# Tabla NA después
eda_types_after <- tibble(
  variable = names(df_miss),
  type     = vapply(df_miss, function(x) class(x)[1], character(1)),
  na_pct   = vapply(df_miss, na_pct, numeric(1))
) |>
  arrange(desc(na_pct))

cat("\nResumen NA antes vs después (join por variable):\n")
na_join <- eda_types_before |>
  select(variable, na_before = na_pct) |>
  inner_join(eda_types_after |>
               select(variable, na_after = na_pct), by = "variable") |>
  arrange(desc(na_before))
print(na_join, n = Inf)

# ================ 4) Outliers: detección y winsorización =========
# Analizar solo numéricas (excluye coordenadas para capado)
outlier_vars <- df_miss |> select(where(is.numeric)) |> select(-latitud, -longitud) |> names()

robust_z <- function(x) (x - median(x, na.rm = TRUE)) / (mad(x, constant = 1.4826, na.rm = TRUE))

iqr_fences <- function(x, mult = 1.5) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  c(lo = q[1] - mult * iqr, hi = q[2] + mult * iqr)
}

winsor_iqr <- function(x, mult = PAR$OUTLIER_RULE) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lo <- q[1] - mult * iqr
  hi <- q[2] + mult * iqr
  x_w <- pmin(pmax(x, lo), hi)
  list(x = x_w, lo = lo, hi = hi)
}

out_tbl <- map_dfr(outlier_vars[-13], function(v) {
  x <- df_miss[[v]]
  f15 <- iqr_fences(x, 1.5); f30 <- iqr_fences(x, 3.0)
  z  <- robust_z(x)
  n15 <- sum(x < f15["lo"] | x > f15["hi"], na.rm = TRUE)
  n30 <- sum(x < f30["lo"] | x > f30["hi"], na.rm = TRUE)
  sk_b <- moments::skewness(x, na.rm = TRUE)
  med_b<- median(x, na.rm = TRUE)
  w    <- winsor_iqr(x, PAR$OUTLIER_RULE)
  sk_a <- moments::skewness(w$x, na.rm = TRUE)
  med_a<- median(w$x, na.rm = TRUE)
  apply_w <- (is.finite(sk_b) && is.finite(sk_a) && (abs(sk_a) <= 0.8*abs(sk_b))) &&
    (is.finite(med_b) && is.finite(med_a) && (abs(med_a - med_b)/max(1e-9, abs(med_b)) <= 0.05))
  tibble(
    variable = v,
    n_out_1_5_iqr = n15,
    n_out_3_0_iqr = n30,
    skew_before = sk_b,
    skew_after  = ifelse(apply_w, sk_a, sk_b),
    median_before = med_b,
    median_after  = ifelse(apply_w, med_a, med_b),
    cap_lo = ifelse(apply_w, w$lo, NA_real_),
    cap_hi = ifelse(apply_w, w$hi, NA_real_),
    winsor_applied = apply_w
  )
}) |>
  arrange(desc(n_out_1_5_iqr))

print(out_tbl, n = Inf)

# Aplicar winsor según la regla
for (v in outlier_vars[-13]) {
  x <- df_miss[[v]]
  w <- winsor_iqr(x, PAR$OUTLIER_RULE)
  sk_b <- moments::skewness(x, na.rm = TRUE)
  med_b<- median(x, na.rm = TRUE)
  sk_a <- moments::skewness(w$x, na.rm = TRUE)
  med_a<- median(w$x, na.rm = TRUE)
  apply_w <- (is.finite(sk_b) && is.finite(sk_a) && (abs(sk_a) <= 0.8*abs(sk_b))) &&
    (is.finite(med_b) && is.finite(med_a) && (abs(med_a - med_b)/max(1e-9, abs(med_b)) <= 0.05))
  if (apply_w) df_miss[[v]] <- w$x
}

# ================ 5) Feature engineering ========================
fe <- df_miss |>
  mutate(
    # tiempo
    anio = year(fecha_inicio),
    mes  = month(fecha_inicio),
    fin_semana = as.integer(wday(fecha_inicio) %in% c(1,7)),
    # diferencias/anomalías
    d_ndvi = ndvi_evt - ndvi_prev,
    d_prcp = prcp_evt - prcp_prev,
    d_tmax = tmax_evt - tmax_prev,
    d_tmin = tmin_evt - tmin_prev,
    d_rh   = rh_evt - rh_prev,
    # orientación: seno/coseno (radianes)
    orient_rad = orientacion * pi / 180,
    orient_sin = sin(orient_rad),
    orient_cos = cos(orient_rad)
  )

# Umbrales percentiles para flags de proximidad
p25_urb   <- quantile(fe$dist_urbano, probs = PAR$FLAG_PERC, na.rm = TRUE)
p25_vias  <- quantile(fe$dist_vias_primarias, probs = PAR$FLAG_PERC, na.rm = TRUE)
p25_ap    <- quantile(fe$dist_areas_protegidas, probs = PAR$FLAG_PERC, na.rm = TRUE)
p25_linea <- quantile(fe$dist_lineas_electricas, probs = PAR$FLAG_PERC, na.rm = TRUE)
p25_rh    <- quantile(fe$rh_evt, probs = PAR$FLAG_PERC, na.rm = TRUE)
p75_tmax  <- quantile(fe$tmax_evt, probs = 1 - PAR$FLAG_PERC, na.rm = TRUE)

fe <- fe |>
  mutate(
    urbano_cerca          = as.integer(dist_urbano < p25_urb),
    vias_cerca            = as.integer(dist_vias_primarias < p25_vias),
    area_protegida_cerca  = as.integer(dist_areas_protegidas < p25_ap),
    linea_electrica_cerca = as.integer(dist_lineas_electricas < p25_linea),
    sequedad_evt_alta     = as.integer(rh_evt < p25_rh & tmax_evt > p75_tmax),
    deficit_ndvi          = as.integer(d_ndvi < 0)
  ) %>% dplyr::select(-fecha)

# Diccionario de nuevas features
features_dict <- tribble(
  ~feature, ~tipo, ~definicion,
  "anio", "int", "Año calendario derivado de fecha_inicio",
  "mes", "int (1-12)", "Mes calendario derivado de fecha_inicio",
  "fin_semana", "binaria (0/1)", "1 si sábado/domingo",
  "d_ndvi", "num", "ndvi_evt - ndvi_prev",
  "d_prcp", "num", "prcp_evt - prcp_prev",
  "d_tmax", "num", "tmax_evt - tmax_prev",
  "d_tmin", "num", "tmin_evt - tmin_prev",
  "d_rh", "num", "rh_evt - rh_prev",
  "orient_sin", "num", "sin(orientación en radianes)",
  "orient_cos", "num", "cos(orientación en radianes)",
  "urbano_cerca", "binaria (0/1)", paste0("1 si dist_urbano < p25 (", round(p25_urb,1), " m)") ,
  "vias_cerca", "binaria (0/1)", paste0("1 si dist_vias_primarias < p25 (", round(p25_vias,1), " m)") ,
  "area_protegida_cerca", "binaria (0/1)", paste0("1 si dist_areas_protegidas < p25 (", round(p25_ap,1), " m)") ,
  "linea_electrica_cerca", "binaria (0/1)", paste0("1 si dist_lineas_electricas < p25 (", round(p25_linea,1), " m)") ,
  "sequedad_evt_alta", "binaria (0/1)", paste0("1 si rh_evt < p25 (", round(p25_rh,1), ") y tmax_evt > p75 (", round(p75_tmax,1), ")"),
  "deficit_ndvi", "binaria (0/1)", "1 si d_ndvi < 0"
)

cat("\nNuevas features creadas:\n"); print(features_dict, n = Inf)

# ================ 6) Filtro técnico previo al modelado ==========
# Selección de continuas candidatas (excluye coordenadas, id, fechas, binarias y categórica)
# continuous_candidates <- fe |>
#   select(where(is.numeric)) |>
#   select(-latitud, -longitud, -fin_semana,
#          -urbano_cerca, -vias_cerca, -area_protegida_cerca, -linea_electrica_cerca,
#          -sequedad_evt_alta, -deficit_ndvi, -cobertura) |>
#   names()



# -------------------- FIN DEL SCRIPT ----------------------------
