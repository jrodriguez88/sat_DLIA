# Feature Engineering para `test_data` (13 pasos)
# Autor: Jrodriguez88 · Fecha: 2025-10-30
# Requisitos: dplyr, tidyr, lubridate, stringr

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
})

#=====================================================
# Utilidades generales
#=====================================================
clip <- function(x, lo = -Inf, hi = Inf) pmin(pmax(x, lo), hi)
clip01 <- function(x) clip(x, 0, 1)
log1p_safe <- function(x) log1p(pmax(x, 0))

winsorize_vec <- function(x, probs = c(0.001, 0.999), na.rm = TRUE) {
  q <- quantile(x, probs = probs, na.rm = na.rm)
  x[x < q[1]] <- as.numeric(q[1])
  x[x > q[2]] <- as.numeric(q[2])
  x
}

# z-score por grupos (devuelve valor estandarizado)
z_by <- function(x, g1, g2) {
  df <- tibble(x = x, g1 = g1, g2 = g2)
  df <- df %>% group_by(g1, g2) %>% mutate(
    mu = mean(x, na.rm = TRUE),
    sdv = sd(x, na.rm = TRUE)
  ) %>% ungroup()
  z <- (df$x - df$mu) / ifelse(is.finite(df$sdv) & df$sdv > 0, df$sdv, NA)
  as.numeric(z)
}

# Indicadores extremos (flag) por percentil dentro de grupo
flag_percentil <- function(x, g1, g2, p = 0.9, type = c("gt","lt")) {
  type <- match.arg(type)
  df <- tibble(x = x, g1 = g1, g2 = g2)
  thr <- df %>% group_by(g1, g2) %>% summarise(
    thr = quantile(x, probs = p, na.rm = TRUE), .groups = "drop"
  )
  df2 <- df %>% left_join(thr, by = c("g1","g2"))
  if (type == "gt") as.integer(df2$x > df2$thr) else as.integer(df2$x < df2$thr)
}

# Imputación robusta por mediana municipio-mes
impute_median_by <- function(x, g1, g2) {
  df <- tibble(x = x, g1 = g1, g2 = g2)
  med <- df %>% group_by(g1, g2) %>% summarise(med = median(x, na.rm = TRUE), .groups = "drop")
  df2 <- df %>% left_join(med, by = c("g1","g2"))
  as.numeric(ifelse(is.na(df2$x), df2$med, df2$x))
}

#=====================================================
# Entrada: data.frame/tibble `test_data`
# (Debe contener las columnas mencionadas por el usuario)
#=====================================================
# Ejemplo de resguardo (no ejecuta si ya existe):
# test_data <- tibble()

# Copia de trabajo
fe <- test_data %>% as_tibble() %>% clean_names()

# write_csv(test_data, "test_data99.csv")

#=====================================================
# Paso 1) Tipos y codificación base
#=====================================================
fe <- fe %>% mutate(
#  municipio    = as.factor(municipio),
  fecha_inicio = as.Date(fecha),
  cobertura    = as.integer(round(cobertura))  # mantener código entero CLC
)

# Sanitizar socioeconómicas (rangos plausibles) / cporcentajes, / indices
fe <- fe %>% mutate(
  educacion = clip(educacion, 0, 100),
  pobreza   = clip(pobreza,   0, 100),
  rh_evt    = clip(rh_evt,    0, 100),
  rh_prev   = clip(rh_prev,   0, 100),
  ndvi_evt  = clip(ndvi_evt,  -1, 1),
  ndvi_prev = clip(ndvi_prev, -1, 1)
)

#=====================================================
# Paso 2) Derivadas temporales (de `fecha_inicio`)
#=====================================================
fe <- fe %>% mutate(
  anio       = year(fecha_inicio),
  mes        = month(fecha_inicio),
  dia_mes    = mday(fecha_inicio),
  dia_semana = wday(fecha_inicio, week_start = 1) - 1,  # 0=Lunes … 6=Domingo
  fin_semana = as.integer(dia_semana %in% c(5,6)),
  trimestre  = quarter(fecha_inicio),
  bimestre   = ((mes - 1) %/% 2) + 1,
  estacion   = case_when(
    mes %in% c(12,1,2) ~ "DJF",
    mes %in% c(3,4,5)  ~ "MAM",
    mes %in% c(6,7,8)  ~ "JJA",
    TRUE               ~ "SON"
  ),
  anio_rel   = anio - 2012
)

#=====================================================
# Paso 3) Transformaciones distancia/terreno
#=====================================================
fe <- fe %>% mutate(
  across(starts_with("dist_"), ~pmax(., 0), .names = "{.col}"),
  across(starts_with("dist_"), log1p_safe, .names = "log1p_{.col}"),
  dist_urbano_lt1km = as.integer(dist_urbano < 1000),
  dist_urbano_1_5km = as.integer(dist_urbano >= 1000 & dist_urbano < 5000),
  dist_urbano_gt5km = as.integer(dist_urbano >= 5000),
  pendiente = pmax(pendiente, 0),
  pendiente_0_5   = as.integer(pendiente < 5),
  pendiente_5_15  = as.integer(pendiente >= 5 & pendiente < 15),
  pendiente_gt15  = as.integer(pendiente >= 15),
  aspect_rad = (orientacion %% 360) * pi/180,
  aspect_sin = sin(aspect_rad),
  aspect_cos = cos(aspect_rad),
  hillshade_norm = sombreado/255
) %>% select(-aspect_rad)

#=====================================================
# Paso 4) Anomalías, deltas y estandarizaciones (municipio×mes)
#=====================================================
# Deltas recientes (evt - prev)
fe <- fe %>% mutate(
  d_ndvi = ndvi_evt - ndvi_prev,
  d_prcp = prcp_evt - prcp_prev,
  d_tmax = tmax_evt - tmax_prev,
  d_tmin = tmin_evt - tmin_prev,
  d_rh   = rh_evt   - rh_prev,
  rr_prcp = (prcp_evt + 1) / (prcp_prev + 1),
  rr_ndvi = (ndvi_evt + 1e-3) / (ndvi_prev + 1e-3)
)

# Z-scores por municipio×mes (climatología 2012–2023)
fe <- fe %>% mutate(
  z_prcp_evt  = z_by(prcp_evt,  municipio, mes),
  z_prcp_prev = z_by(prcp_prev, municipio, mes),
  z_tmax_evt  = z_by(tmax_evt,  municipio, mes),
  z_tmax_prev = z_by(tmax_prev, municipio, mes),
  z_tmin_evt  = z_by(tmin_evt,  municipio, mes),
  z_tmin_prev = z_by(tmin_prev, municipio, mes),
  z_rh_evt    = z_by(rh_evt,    municipio, mes),
  z_rh_prev   = z_by(rh_prev,   municipio, mes),
  z_ndvi_evt  = z_by(ndvi_evt,  municipio, mes),
  z_ndvi_prev = z_by(ndvi_prev, municipio, mes)
)

# Flags de extremos estacionales
fe <- fe %>% mutate(
  tmax_evt_p90_flag = flag_percentil(tmax_evt, municipio, mes, p = 0.90, type = "gt"),
  prcp_evt_p10_flag = flag_percentil(prcp_evt, municipio, mes, p = 0.10, type = "lt"),
  ndvi_evt_p10_flag = flag_percentil(ndvi_evt, municipio, mes, p = 0.10, type = "lt")
)

#=====================================================
# Paso 5) Índices climáticos/energéticos simples
#=====================================================
fe <- fe %>% mutate(
  dtr_evt  = tmax_evt - tmin_evt,
  dtr_prev = tmax_prev - tmin_prev,
  ai_evt   = tmax_evt / (rh_evt + 1),
  ai_prev  = tmax_prev / (rh_prev + 1),
  vpd_proxy_evt  = tmax_evt  * (1 - rh_evt/100),
  vpd_proxy_prev = tmax_prev * (1 - rh_prev/100),
  dry_spell_flag = as.integer(prcp_evt < quantile(prcp_evt, 0.10, na.rm = TRUE) & d_ndvi < 0)
)

#=====================================================
# Paso 6) Vegetación (NDVI)
#=====================================================
fe <- fe %>% mutate(
  ndvi_media_2m   = (ndvi_prev + ndvi_evt)/2,
  greenup_flag    = as.integer(d_ndvi >  0.05),
  senescence_flag = as.integer(d_ndvi < -0.05),
  fine_fuel_proxy = ndvi_prev * (1/(1 + prcp_evt))  # mayor si venía verde y luego llueve poco
)

#=====================================================
# Paso 7) Cobertura CLC – macroclases e inflamabilidad
#=====================================================
# Macroclase por dígito centena (1 artificial, 2 agrícola, 3 forestal/seminatural, 4 humedales, 5 agua)
fe <- fe %>% mutate(
  clc_macro = as.integer(floor(cobertura/10)),
  macro = case_when(
    clc_macro == 1 ~ "urbano",
    clc_macro == 2 ~ "agricola",
    clc_macro == 3 ~ "bosque",
    clc_macro == 4 ~ "humedal",
    clc_macro == 5 ~ "agua",
    TRUE ~ "otros"
  ),
  macro_urbano_flag  = as.integer(macro == "urbano"),
  macro_agricola_flag= as.integer(macro == "agricola"),
  macro_bosque_flag  = as.integer(macro == "bosque"),
  macro_humedal_flag = as.integer(macro == "humedal"),
  macro_agua_flag    = as.integer(macro == "agua")
)

# Regla simple de inflamabilidad (editable)
fe <- fe %>% mutate(
  inflamabilidad_clase = case_when(
    macro %in% c("agricola") ~ "alta",
    macro %in% c("bosque")   ~ "media",
    macro %in% c("urbano","humedal","agua") ~ "baja",
    TRUE ~ "media"
  )
)

#=====================================================
# Paso 8) Socioeconómicas – tasas e índices ponderados
#=====================================================
fe <- fe %>% mutate(
  area_afectada_per10k = area_afectada / (poblacion/10000),
  educacion_01 = clip01(educacion/100),
  pobreza_01   = clip01(pobreza/100),
  access_proxy = 1/(1 + dist_vias_primarias),
  urb_pop_exposure = poblacion * 1/(1 + dist_urbano)
)

#=====================================================
# Paso 9) Interacciones candidatas (parsimoniosas)
#=====================================================
fe <- fe %>% mutate(
  inter_dndvi_ztmax = d_ndvi * z_tmax_evt,
  inter_dndvi_zprcp = d_ndvi * z_prcp_evt,
  inter_access_pastos = access_proxy * as.integer(macro == "agricola"),
  inter_pend_bosque   = pendiente * macro_bosque_flag,
  inter_distprot_infl = dist_areas_protegidas * as.integer(inflamabilidad_clase == "alta")
)

#=====================================================
# Paso 10) Control de calidad, imputación y banderas NA
#=====================================================
# Banderas de NA originales
na_cols <- c("prcp_evt","prcp_prev","tmax_evt","tmax_prev","tmin_evt","tmin_prev","rh_evt","rh_prev",
             "ndvi_evt","ndvi_prev")
for (cc in na_cols) fe[[paste0(cc,"_miss_flag")]] <- as.integer(is.na(fe[[cc]]))

# Imputación por mediana municipio×mes
for (cc in na_cols) fe[[cc]] <- impute_median_by(fe[[cc]], fe$municipio, fe$mes)

# Winsorización de variables muy asimétricas
wins_cols <- c(
  grep("^dist_", names(fe), value = TRUE),
  "poblacion","area_afectada","urb_pop_exposure"
)
for (cc in wins_cols) fe[[cc]] <- winsorize_vec(fe[[cc]])

#=====================================================
# Transformación opcional del objetivo (severidad)
#=====================================================
# Si modelas severidad (regresión sobre área), la distribución es muy sesgada.
# Guarda una versión logarítmica segura para ceros y valores pequeños.
fe <- fe %>% mutate(
  area_log1p = log1p(pmax(area_afectada, 0))
)

# Nota:
# - Si usas modelos lineales en `area_log1p`, interpreta coeficientes de forma multiplicativa.
# - Al predecir y volver a la escala original, usa corrección de sesgo (smearing de Duan).
# - Alternativas: GLM Gamma con enlace log, Tweedie, o modelos hurdle/ZI si hay muchos ceros.

#=====================================================
# Paso 11) Paquete mínimo viable (subset sugerido)
#=====================================================
vars_mvp <- c(
  # calendario
  "mes","fin_semana","estacion","anio_rel",
  # estáticos
  grep("^log1p_dist_", names(fe), value = TRUE),
  c("pendiente","aspect_sin","aspect_cos","hillshade_norm","elevacion"),
  # cobertura
  c("macro_urbano_flag","macro_agricola_flag","macro_bosque_flag","macro_humedal_flag","macro_agua_flag","inflamabilidad_clase"),
  # clima/vegetación (sin fuga para ocurrencia: usar *_prev + deltas)
  c("ndvi_prev","d_ndvi","z_tmax_prev","z_prcp_prev","vpd_proxy_prev","dtr_prev","prcp_prev"),
  # socioeconómico
  c("access_proxy","urb_pop_exposure","pobreza_01","educacion_01")
)

mvp <- fe %>% select(any_of(c("latitud", "longitud", "area_afectada","fecha_inicio","municipio", vars_mvp)))

#=====================================================
# Paso 12) Nomenclatura (ya aplicada en creación de variables)
#=====================================================
# Convenciones usadas: d_* (delta), rr_* (razón), z_* (z-score), *_flag (binaria), log1p_* (log), *_01 (0–1)

#=====================================================
# Paso 13) Checklist automatizado (diagnósticos básicos)
#=====================================================
# 13.1 Rangos clave
chk <- list(
  rango_ndvi = summary(select(fe, ndvi_prev, ndvi_evt)),
  rango_rh   = summary(select(fe, rh_prev, rh_evt)),
  rango_soc  = summary(select(fe, educacion, pobreza, poblacion))
)

# 13.2 Colinealidad rápida (correlación numérica)
num_cols <- fe %>% select(where(is.numeric)) %>% names()
cor_matriz <- fe %>% select(any_of(num_cols)) %>% mutate(across(everything(), as.numeric)) %>%
  cor(use = "pairwise.complete.obs")

corrplot::corrplot(cor_matriz)

# 13.3 Split temporal train/test (ej.: hasta 2021 / desde 2022)
train <- fe %>% filter(anio <= 2021)
test  <- fe %>% filter(anio >= 2022)

# 13.4 Exportables en objeto de salida
fe_output <- list(
  features_full = fe,
  features_mvp  = mvp,
  checks        = chk,
  cor_matrix    = cor_matriz,
  split         = list(train = train, test = test)
)

# Resultado principal: `fe_output`
# - fe_output$features_full : tabla con todas las variables derivadas
# - fe_output$features_mvp  : subset mínimo viable
# - fe_output$checks        : resúmenes rápidos
# - fe_output$cor_matrix    : matriz de correlación
# - fe_output$split         : listas train/test por tiempo



