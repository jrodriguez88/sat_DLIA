library(dplyr)
library(tidymodels)

# Índice de filas completas
rows_complete <- complete.cases(fe_pca_cluster)
idx_complete  <- which(rows_complete)

# VARIABLES PREDICTORAS (las que usaste en PCA/cluster)
vars_pca_cluster <- colnames(fe_pca_cluster)

# Data para modelar: solo filas completas, outcome = cluster_kmeans
data_model <- fe_clusters[idx_complete, ] %>%
#  select(cluster_kmeans, all_of(vars_pca_cluster)) %>%
#  select(cluster_kmeans, all_of(vars_pca_cluster)) %>%
  mutate(cluster_kmeans = factor(cluster_kmeans)) #%>%
 # filter
#  dplyr::select(-contains("_afectada"), - cluster_kmeans)



set.seed(123)

data_split <- initial_split(
  data_model,
  prop   = 0.7,
  strata = area_log1p
)

train_data <- training(data_split)
test_data  <- testing(data_split)

set.seed(123)
folds <- vfold_cv(
  train_data,
  v      = 5,
  strata = area_log1p
)


# REGRESIÓN: predice area_afectada_log. No usa cluster_k como predictor.
rec_reg <- # REGRESIÓN: predice area_log1p con TODO lo demás como predictor
  rec_reg <- recipe(area_log1p ~ ., data = train_data) %>%
  # features temporales a partir de fecha_inicio
  step_date(
    fecha_inicio,
    features = c("dow", "month", "quarter", "semester")
  ) %>% 
  step_interact(
    terms = ~ d_ndvi:(d_tmax + d_prcp + d_rh + vpd_proxy_evt) +
      fine_fuel_proxy:(d_tmax + ai_evt + vpd_proxy_evt) +
      pendiente:(d_tmax + ai_evt)
  ) %>% 
  step_corr() %>%
 # step_lo
  # eliminar columnas que NO quieres como predictores
  step_rm(
    fecha, fecha_inicio,             # ya las convertimos a features
    cluster_kmeans,                  # no usar el cluster como predictor
    orientacion, orient_rad,          # usas orient_sin / orient_cos
    # si no quieres usar area_afectada como predictor directo:
    contains("_afectada"), 
    area_afectada_per10k, educacion, pobreza,# area_log1p,
    contains("^anio"), contains("cluster"),
    contains("01$")
  ) %>%
  step_novel(all_nominal_predictors()) %>%  
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  # quitar varianza cero
  step_zv(all_predictors()) %>%
  # imputación automática
  step_impute_knn(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  # normalizar numéricas (esto NO crea columnas z_*, solo reescala in-place)
  step_normalize(all_numeric_predictors()) %>%
  # dummies para categóricas
  step_dummy(all_nominal_predictors(), one_hot = TRUE)



rf_spec_reg <- rand_forest(
  mtry  = tune(),
  min_n = tune(),
  trees = 500
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")


## Regresion
rf_wf <- workflow() %>%
  add_model(rf_spec_reg) %>%
  add_recipe(rec_reg)



set.seed(123)
rf_tune <- tune_grid(
  rf_wf,
  resamples = folds,
  grid      = 20,
  metrics   = metric_set(rmse, mae, rsq)
) 

# ver primero las métricas, opcional
collect_metrics(rf_tune)

best_rf <- select_best(rf_tune, metric = "rmse")

rf_final_wf <- finalize_workflow(rf_wf, best_rf)

# Ajuste final en train
rf_fit <- fit(rf_final_wf, data = train_data)

# Predicciones en test
rf_preds <- predict(rf_fit, new_data = test_data) %>%
  bind_cols(test_data %>% select(area_log1p))

# Métricas
rf_metrics <- rf_preds %>%
  metrics(truth = area_log1p, estimate = .pred)

rf_metrics


library(vip)

rf_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 50)



# Perfil de clúster (medianas de algunas variables clave)
profile_vars <- c("area_afectada", "poblacion", "pobreza", "elevacion","prcp_evt", "tmin_evt", "rh_evt",
                  "tmax_evt", "dist_areas_protegidas", "dist_urbano" ,"d_prcp","d_tmax", "d_tmin", "d_rh","d_ndvi")
cluster_profile <- fe_clusters |>
  group_by(cluster_kmeans) |>
  summarise(across(all_of(profile_vars), median, na.rm = TRUE), .groups = "drop")

cat("\nPerfil de clúster (medianas):\n"); print(cluster_profile)

