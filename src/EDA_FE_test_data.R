# ---------------------------------------------------------------
# Pipeline reproducible de análisis estadístico para `test_data`
# Incendios forestales (CAR  — R script
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
  PATH                = "data/final/test_data.csv",   # <- sustituye si tu ruta es distinta
  KNN_K               = 5,
  OUTLIER_RULE        = 50,                # 3×IQR para capado/winsor
  CORR_THRESHOLD      = 0.95,               # |r| > 0.97 => eliminar 1 de cada par
  VAR_THRESHOLD_ZVAR  = 0.10,               # elimina var(z) <= 0.10 (si aplica)
  FLAG_PERC           = 0.25,               # percentil 25 para proximidades
  PCA_VAR_TARGET      = 0.65,               # % acumulado para truncar PCs
  K_RANGE             = 2:8,                # rango de k para clustering
  RUN_FAMD            = TRUE,              # TRUE si quieres comparar con FAMD
  RUN_CLUSTGEO        = FALSE               # TRUE si ponderas espacio (requiere clustGeo)
)

# ================ 1) Carga y tipificación =======================
stopifnot(file.exists(PAR$PATH))
raw <- readr::read_csv(PAR$PATH, show_col_types = FALSE) |> clean_names()

# raw <- fe_output$features_full |> clean_names()

# Asegurar CRS: jurisdicción → CRS de eventos
crs_ev <- sf::st_crs(eventos_car)
juris_proj <- sf::st_transform(jurisdiccion_car, crs_ev) |> sf::st_make_valid()
elevacion_car <- rast_terrain[[1]] %>% project(juris_proj)



# Coerción mínima garantizada
expected_cols <- c(
  "latitud", "longitud", "municipio", "fecha", "area_afectada",
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
    cobertura    = as.integer(cobertura)#,
    # municipio    = as.character(municipio)
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
  )

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


# 1) Quedarse solo con columnas numéricas
fe_num <- fe %>%
  select(where(is.numeric))

# 2) Quitar a mano cosas que NO quieres en PCA / clustering
#    (IDs, coords, tiempo, códigos de clase, ángulos crudos)
cols_quitar_manualmente <- c(
  "objectid", "latitud", "longitud",
  "anio", "mes", "dia_mes", "dia_semana",
  "fin_semana", "trimestre", "bimestre", "anio_rel",
  "cobertura", "clc_macro",
  "orient_rad", "orient_cos", "orient_sin"
)

fe_num2 <- fe_num %>%
  select(-any_of(cols_quitar_manualmente))

# 3) Quitar variables discretas tipo dummies / clases
#    (pocas categorías: <= 5 valores distintos)
fe_continuas <- fe_num %>%
  select(where(~ dplyr::n_distinct(.x, na.rm = TRUE) > 5))

# 4) Este es tu set para PCA y clustering
vars_pca_cluster <- names(fe_continuas)

fe_pca_cluster <- fe %>%
  select(all_of(vars_pca_cluster))

# Varianza en z-escala (opcional; no suele eliminar en práctica)
Z <- scale(fe_pca_cluster)
var_z <- apply(Z, 2, var)
vars_lowvar <- names(var_z[var_z <= PAR$VAR_THRESHOLD_ZVAR])

if (length(vars_lowvar)) {
  message("Eliminando por var(z) <= ", PAR$VAR_THRESHOLD_ZVAR, ": ", paste(vars_lowvar, collapse=", "))
}

cont_after_var <- setdiff(names(fe_pca_cluster), vars_lowvar)

# Colinealidad |r| > 0.85 (greedy por redundancia promedio)
find_redundant <- function(df_num, thr = 0.97) {
  CM <- abs(cor(scale(df_num), use = "pairwise.complete.obs"))
  diag(CM) <- 0
  drops <- character(0)
  while (TRUE) {
    mx <- max(CM, na.rm = TRUE)
    if (!is.finite(mx) || mx <= thr) break
    ij <- which(CM == mx, arr.ind = TRUE)[1, ]
    v1 <- colnames(CM)[ij[1]]; v2 <- colnames(CM)[ij[2]]
    m1 <- mean(CM[v1, -which(colnames(CM) == v1)], na.rm = TRUE)
    m2 <- mean(CM[v2, -which(colnames(CM) == v2)], na.rm = TRUE)
    drop_v <- ifelse(m1 >= m2, v1, v2)
    drops <- c(drops, drop_v)
    keep <- setdiff(colnames(CM), drop_v)
    CM <- CM[keep, keep, drop = FALSE]
  }
  unique(drops)
}

redundant_vars <- find_redundant(fe[cont_after_var], thr = PAR$CORR_THRESHOLD)

if (length(redundant_vars)) {
  message("Eliminando por colinealidad |r| > ", PAR$CORR_THRESHOLD, ": ", paste(redundant_vars, collapse=", "))
}

cont_final <- setdiff(cont_after_var, redundant_vars)

cat("\nVariables eliminadas (NA/varianza/colinealidad):\n")
print(list(
  por_na_25 = vars_drop_na,
  por_varianza = vars_lowvar,
  por_colinealidad = redundant_vars
))

# ================ 7) PCA (solo continuas) =======================
X <- scale(fe[c(cont_final)])#, "latitud", "longitud")])
pca <- prcomp(X, center = TRUE, scale. = FALSE) 

# Varianza explicada
var_exp <- tibble(
  PC = paste0("PC", seq_along(pca$sdev)),
  eigen = pca$sdev^2,
  prop = (pca$sdev^2) / sum(pca$sdev^2)
) |>
  mutate(prop_acum = cumsum(prop))

print(var_exp |> slice_head(n = 10))

# Nº de PCs para alcanzar objetivo
n_pc <- which(var_exp$prop_acum >= PAR$PCA_VAR_TARGET)[1]
message("PCs necesarios para ", PAR$PCA_VAR_TARGET*100, "% var: ", n_pc)

# Cargas (top 10 por |loading| para las 5 primeras PCs)
loadings <- as_tibble(pca$rotation, rownames = "variable")
load_top <- map_dfr(1:min(5, ncol(loadings)-1), function(i) {
  loadings |>
    select(variable, !!sym(paste0("PC", i))) |>
    rename(loading = !!sym(paste0("PC", i))) |>
    mutate(PC = paste0("PC", i), abs_loading = abs(loading)) |>
    arrange(desc(abs_loading)) |>
    slice_head(n = 10)
})

cat("\nTop 10 cargas por PC (1-5):\n"); print(load_top, n = Inf)

# Biplot (opcional: descomenta para ver)
# factoextra::fviz_pca_biplot(pca, repel = TRUE, label = "var")


factoextra::fviz_eig(pca, addlabels = TRUE, barfill = "steelblue") +
  labs(title = "Varianza explicada por componente")

factoextra::fviz_contrib(pca, choice = "var", , axes = c(1:8), top = 20)

factoextra::fviz_pca_var(pca, col.var = "contrib",
                         gradient.cols = c("white", "blue", "red"),
                         ggtheme = theme_minimal(), axes = c(1, 2), 
                         repel = TRUE)


factoextra::fviz_pca_var(pca, col.var = "contrib",
                         gradient.cols = c("white", "blue", "red"),
                         ggtheme = theme_minimal(), axes = c(3, 4), 
                         repel = TRUE)




# ================ 8) Clustering (dos rutas) =====================
clust_metrics <- tibble()

# Ruta A: Ward.D2 sobre continuas estandarizadas
D  <- dist(X, method = "euclidean")
hc <- hclust(D, method = "ward.D2")
for (k in PAR$K_RANGE) {
  cl <- cutree(hc, k)
  sil <- silhouette(cl, D)
  sil_mean <- mean(sil[, 3])
  dbi <- intCriteria(as.matrix(X), as.integer(cl), c("Davies_Bouldin"))$davies_bouldin
  clust_metrics <- bind_rows(clust_metrics, tibble(ruta = "WardD2", k = k, silhouette = sil_mean, DBI = dbi))
}

# Ruta B: K-means sobre PCs (>=80% var.)
Xpc <- pca$x[, 1:n_pc, drop = FALSE]

Xpc <- X


for (k in PAR$K_RANGE) {
  km <- kmeans(Xpc, centers = k, nstart = 50, iter.max = 500)
  sil <- silhouette(km$cluster, dist(Xpc))
  sil_mean <- mean(sil[, 3])
  dbi <- intCriteria(as.matrix(Xpc), as.integer(km$cluster), c("Davies_Bouldin"))$davies_bouldin
  clust_metrics <- bind_rows(clust_metrics, tibble(ruta = "kmeans_PC", k = k, silhouette = sil_mean, DBI = dbi))
}

cat("\nMétricas de clustering (silhouette ↑ mejor, DBI ↓ mejor):\n")
print(clust_metrics |> arrange(ruta, k))

# Curvas Silhouette y DBI vs k
p_sil <- ggplot(clust_metrics, aes(x = k, y = silhouette, color = ruta, group = ruta)) + geom_line() + geom_point() + labs(title = "Silhouette vs k", x = "k", y = "Silhouette") + theme_minimal(base_size = 12)
p_dbi <- ggplot(clust_metrics, aes(x = k, y = DBI, color = ruta, group = ruta)) + geom_line() + geom_point() + labs(title = "DBI vs k (menor es mejor)", x = "k", y = "DBI") + theme_minimal(base_size = 12)
p_sil; p_dbi

# Selección automática (ejemplo: mejor DBI; si empate, mayor silhouette)
sel <- clust_metrics |>
  group_by(ruta) |>
  arrange(DBI, desc(silhouette), .by_group = TRUE) |>
  dplyr::slice(1) |>
  ungroup() |>
  arrange(DBI, desc(silhouette)) #|>
 # dplyr::slice(1)

cat("\nSelección propuesta:\n"); print(sel)

  best_k <- sel %>% filter(sel$ruta == "WardD2") %>% pull(k)
  clusters_Ward <- cutree(hc, best_k)

  best_k <- sel %>% filter(sel$ruta == "kmeans_PC") %>% pull(k)
  clusters_kmeans <- kmeans(Xpc, centers = best_k, nstart = 50, iter.max = 100)$cluster


fe$cluster_k <- clusters_kmeans

# Perfil de clúster (medianas de algunas variables clave)
profile_vars <- c("area_afectada", "poblacion", "pobreza", "elevacion","prcp_evt", "tmin_evt", "rh_evt",
                  "tmax_evt", "dist_areas_protegidas", "dist_urbano" ,"d_prcp","d_tmax", "d_tmin", "d_rh","d_ndvi")
cluster_profile <- fe |>
  group_by(cluster_k) |>
  summarise(across(all_of(profile_vars), median, na.rm = TRUE), .groups = "drop")

cat("\nPerfil de clúster (medianas):\n"); print(cluster_profile)

fe |>
  group_by(cluster_k) |>
  summarise(n(),
            area_total = sum(area_afectada), .groups = "drop") %>% left_join(cluster_profile)

# ================ 9) Resumen de decisiones ======================
decisiones <- list(
  seed = 42,
  umbral_na_25 = vars_drop_na,
  imputacion = list(numericas = paste0("KNN k=", PAR$KNN_K, " (estandarizado interno)"), categoricas = "moda para cobertura"),
  outliers = list(regla = paste0(PAR$OUTLIER_RULE, "×IQR"), criterio_aplicar = "reduce |skew| >=20% y |Δmediana| <5%"),
  varianza = paste0("eliminadas var(z) <= ", PAR$VAR_THRESHOLD_ZVAR),
  colinealidad = paste0("|r| > ", PAR$CORR_THRESHOLD, " (greedy por redundancia)"),
  pca = list(vars = cont_final, pcs_para_80 = n_pc),
  clustering = sel
)

cat("\nDecisiones y trazabilidad:\n"); print(decisiones)

ggplot(fe, aes(x = longitud, y = latitud, color = as.factor(cluster_k))) +
  geom_point(alpha = 0.45, size = 1.2, na.rm = TRUE) +
  coord_equal() +
  guides(color = guide_legend(title = "Cluster")) +
  labs(x = "Longitud (m)", y = "Latitud (m)", title = "Distribución espacial de eventos (EPSG:9377)") +
  theme_minimal(base_size = 12)


# ================ 10) (Opcional) FAMD / clustGeo =================
if (isTRUE(PAR$RUN_FAMD)) {
  message("Ejecutando FAMD (continuas + categ/flags)…")
  # Selección ejemplo: continuas finales + cobertura + flags binarios
  famd_df <- fe |>
    select(all_of(cont_final), cobertura,
           urbano_cerca, vias_cerca, area_protegida_cerca, linea_electrica_cerca,
           sequedad_evt_alta, deficit_ndvi) 
  famd_df$cobertura <- factor(famd_df$cobertura)
  famd <- FactoMineR::FAMD(famd_df, ncp = 10, graph = FALSE)
  print(famd$eig)
}

if (isTRUE(PAR$RUN_CLUSTGEO)) {
  message("clustGeo no activado por defecto; requiere paquete `ClustGeo` y tuning de lambda.")
}




# ================ 11) Exportes ligeros (opcional) ===============
# Descomenta si quieres CSVs de tablas clave en el wd
# readr::write_csv(na_join, "NA_summary_before_after.csv")
# readr::write_csv(out_tbl, "outliers_actions.csv")
# readr::write_csv(features_dict, "features_dictionary.csv")
# readr::write_csv(var_exp, "pca_variance_explained.csv")
# readr::write_csv(load_top, "pca_top_loadings.csv")
# readr::write_csv(clust_metrics, "clustering_metrics.csv")
# readr::write_csv(cluster_profile, "cluster_profile_medians.csv")

# ================ 8.1) Figuras clave ============================
# Histogramas + densidad para variables clave
plot_hist_density <- function(data, var, bins = 30) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = bins, alpha = 0.4) +
    geom_density(linewidth = 0.8) +
    labs(x = var, y = "Densidad") +
    theme_minimal(base_size = 12)
}

vars_area <- c("area_afectada")
vars_dist  <- c("dist_urbano","dist_areas_protegidas","dist_drenaje_doble",
                "dist_vias_primarias","dist_embalses","dist_lineas_electricas")
vars_ndvi  <- c("ndvi_evt","ndvi_prev")
vars_clima <- c("prcp_evt","prcp_prev","tmax_evt","tmax_prev",
                "tmin_evt","tmin_prev","rh_evt","rh_prev")
vars_terr  <- c("elevacion","pendiente", "orientacion", "sombreado")

message("
=== Histogramas/densidades ===")
walk(vars_area, ~ print(plot_hist_density(fe, .x, bins = 50)))
walk(vars_dist,  ~ print(plot_hist_density(fe, .x)))
walk(vars_ndvi,  ~ print(plot_hist_density(fe, .x)))
walk(vars_clima, ~ print(plot_hist_density(fe, .x)))
walk(vars_terr,  ~ print(plot_hist_density(fe, .x)))

# Mapa de calor de correlaciones (post-limpieza)
num_df_clean <- fe %>% select(where(is.numeric)) %>% select(-latitud, -longitud)
corP2 <- cor(num_df_clean, use = "pairwise.complete.obs", method = "pearson")
corS2 <- cor(num_df_clean, use = "pairwise.complete.obs", method = "spearman")

message("
=== Heatmap de correlación (Pearson) ===")
print( ggcorrplot(corP2, type = "lower", lab = FALSE, outline.col = "white") +
         ggtitle("Correlación Pearson (post-limpieza)") )

message("
=== Heatmap de correlación (Spearman) ===")
print( ggcorrplot(corS2, type = "lower", lab = FALSE, outline.col = "white") +
         ggtitle("Correlación Spearman (post-limpieza)") )

# Biplot PCA PC1–PC2 con variables (y clusters si existen)
p_biplot <- factoextra::fviz_pca_biplot(
  pca, repel = TRUE, label = "var",
  habillage = as.factor(fe$cluster_k), addEllipses = TRUE
)
print(p_biplot)

factoextra::fviz_pca_biplot(
  pca, repel = TRUE, label = "var",
  habillage = as.factor(fe$cluster_k), addEllipses = TRUE, axes = c(3,4)
)

# Curvas silhouette y DBI vs k
p_sil <- ggplot(clust_metrics, aes(x = k, y = silhouette, color = ruta, group = ruta)) +
  geom_line() + geom_point() +
  labs(title = "Silhouette vs k", x = "k", y = "Silhouette") +
  theme_minimal(base_size = 12)

p_dbi <- ggplot(clust_metrics, aes(x = k, y = DBI, color = ruta, group = ruta)) +
  geom_line() + geom_point() +
  labs(title = "DBI vs k (menor es mejor)", x = "k", y = "DBI") +
  theme_minimal(base_size = 12)

print(p_sil + p_dbi)

# ================ 12) Session info ==============================
cat("\n\nSessionInfo():\n"); print(sessionInfo())

# -------------------- FIN DEL SCRIPT ----------------------------
