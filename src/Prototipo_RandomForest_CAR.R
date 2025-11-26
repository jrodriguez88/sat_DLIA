library(dplyr)
library(tidymodels)

# Índice de filas completas
rows_complete <- complete.cases(fe)
idx_complete  <- which(rows_complete)

# VARIABLES PREDICTORAS (las que usaste en PCA/cluster)
vars_pca_cluster <- colnames(fe)

# Data para modelar: solo filas completas, outcome = cluster_kmeans
data_model <- fe[idx_complete, ] %>% 
  select(y, dist_urbano, rh_evt, tmax_evt, dist_vias_primarias, pobreza, poblacion,
         elevacion, dist_embalses, ndvi_evt, tmin_evt, dist_drenaje_doble, dist_areas_protegidas, 
         educacion, prcp_evt, pendiente, dist_lineas_electricas, orientacion, sombreado) %>% 
  # select(y, dist_urbano, rh_prev, tmax_prev, dist_vias_primarias,
  #        elevacion, dist_embalses, ndvi_prev, tmin_prev, dist_drenaje_doble, 
  #        educacion, prcp_prev, pendiente, dist_lineas_electricas) %>%
  # select(y, fecha_inicio, dist_urbano, rh_evt, tmax_evt, dist_vias_primarias,
  #        elevacion, dist_embalses, ndvi_evt, tmin_evt, dist_drenaje_doble, 
  #        educacion, prcp_evt, pendiente, dist_lineas_electricas) %>%
  mutate(
    #cobertura = as.factor(cobertura), 
         y = as.factor(y)) #%>%select(all_of(top_features))#%>%
  #  select(cluster_kmeans, all_of(vars_pca_cluster)) %>%
  #  select(cluster_kmeans, all_of(vars_pca_cluster)) %>%
  #mutate(cluster_kmeans = factor(cluster_kmeans)) #%>%
# filter
#  dplyr::select(-contains("_afectada"), - cluster_kmeans)



set.seed(123)

data_split <- initial_split(
  data_model,
  prop   = 0.7,
  strata = NULL
)

train_data <- training(data_split)
test_data  <- testing(data_split)

set.seed(123)
folds <- vfold_cv(
  train_data,
  v      = 5,
  strata = NULL
)


# REGRESIÓN: predice area_afectada_log. No usa cluster_k como predictor.
rec_reg <- # REGRESIÓN: predice area_log1p con TODO lo demás como predictor
  rec_reg <- recipe(y ~ ., data = train_data) %>%
  # features temporales a partir de fecha_inicio
  # step_date(
  #   fecha_inicio,
  #   features = c("dow", "month", "quarter", "semester")
  # ) %>% 
  # step_interact(
  #   terms = ~ d_ndvi:(d_tmax + d_prcp + d_rh + vpd_proxy_evt) +
  #     fine_fuel_proxy:(d_tmax + ai_evt + vpd_proxy_evt) +
  #     pendiente:(d_tmax + ai_evt)
  # ) %>% 
  step_corr() %>%
  # step_lo
  # eliminar columnas que NO quieres como predictores
  step_rm(
    contains("fecha"),             # ya las convertimos a features
 #   contains("orientacion"),
    #cluster_kmeans,                  # no usar el cluster como predictor
    #orientacion, orient_rad,          # usas orient_sin / orient_cos
    # si no quieres usar area_afectada como predictor directo:
    contains("_afectada"), 
    #educacion, pobreza,# area_log1p,
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



rf_spec_class <- rand_forest(
  mtry  = tune(),
  min_n = tune(),
  trees = 1000
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("classification")


## Regresion
rf_wf <- workflow() %>%
  add_model(rf_spec_class) %>%
  add_recipe(rec_reg)



set.seed(123)
rf_tune <- tune_grid(
  rf_wf,
  resamples = folds,
  grid      = 20,
  metrics   = metric_set(f_meas, precision, recall, specificity, roc_auc)
) 

# ver primero las métricas, opcional
collect_metrics(rf_tune)

best_rf <- select_best(rf_tune, metric = "f_meas")

rf_final_wf <- finalize_workflow(rf_wf, best_rf)

# Ajuste final en train
rf_fit <- fit(rf_final_wf, data = train_data)

# Predicciones en test
rf_preds <- predict(rf_fit, new_data = test_data) %>%
   bind_cols(test_data %>% select(y))

# Métricas
rf_metrics <- rf_preds %>%
  metrics(truth = y, estimate = .pred_class)

rf_metrics

rf_preds %>%
  conf_mat(truth = y, estimate = .pred_class)


library(vip)

rf_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 50)


# train %>% 
#   select(y, dist_urbano, rh_prev, tmax_prev, dist_vias_primarias, elevacion, dist_embalses, 
#          ndvi_prev, tmin_prev, dist_drenaje_doble, educacion, prcp_prev, pendiente, dist_lineas_electricas)

