
# 1. Librerías y plantilla ----

library(terra)
library(sf)
library(dplyr)
library(tidymodels)   # para usar predict() sobre rf_fit

# Plantilla de resolución/extent/CRS
# Plantilla original (~100 m)
template_100m <- dist_urbano

# Agregar a ~1000 m (100 m * 10 = 1 km aprox)
template_1km <- aggregate(
  template_100m,
  fact = factor_resol,      # factor de agregación (cambia 10 por 5, 20, etc. según lo que quieras)
  fun  = mean     # da igual la función, solo usamos geometría
)

# A partir de aquí, usa esta como plantilla en TODO el flujo
template <- template_1km
crs(template)


# 2. Preparar terreno (elevación, orientación, pendiente, sombreado) ----
# rast_terrain es una lista de 4 SpatRaster
terrain_stack <- rast(rast_terrain)  # combina en un solo SpatRaster multilayer

terrain_stack
names(terrain_stack)
# "elevation" "Orientación" "Pendiente" "Sombreado de relieve"

# Re-muestrear a la plantilla
terrain_res <- resample(terrain_stack, template, method = "bilinear")

# Renombrar para que coincida con data_model
names(terrain_res) <- c("elevacion", "orientacion", "pendiente", "sombreado")
terrain_res


# 3. Rasterizar variables socioeconómicas ----
# Asegurar nombres simples para pobreza, poblacion, educacion
socio_df <- socioeconomic_data_spatial |>
  mutate(
    pobreza   = `Pobreza (%) 2023`,
    poblacion = `Población (hab) 2025`,
    educacion = `Educación (% Cobertura)`
  )

# Convertir a SpatVector
socio_vect <- vect(socio_df)

# Rasterizar cada variable sobre la plantilla
r_pobreza <- rasterize(socio_vect, template, field = "pobreza",   fun = "mean")
r_poblac  <- rasterize(socio_vect, template, field = "poblacion", fun = "mean")
r_educ    <- rasterize(socio_vect, template, field = "educacion", fun = "mean")

names(r_pobreza) <- "pobreza"
names(r_poblac)  <- "poblacion"
names(r_educ)    <- "educacion"

# 4. Construir stack de predictores estáticos ----
# Aquí asumo que ya tienes:
# dist_urbano, dist_vias_primarias, dist_embalses,
# dist_drenaje_doble, dist_areas_protegidas, dist_lineas_electricas

# Si no están alineados, puedes asegurarlo así (mismo CRS, extent y resolución):
dist_urbano           <- resample(dist_urbano,           template, method = "bilinear")
dist_vias_primarias   <- resample(dist_vias_primarias,   template, method = "bilinear")
dist_embalses         <- resample(dist_embalses,         template, method = "bilinear")
dist_drenaje_doble    <- resample(dist_drenaje_doble,    template, method = "bilinear")
dist_areas_protegidas <- resample(dist_areas_protegidas, template, method = "bilinear")
dist_lineas_electricas<- resample(dist_lineas_electricas,template, method = "bilinear")

# Stack estático completo
static_stack <- c(
  dist_urbano,
  dist_vias_primarias,
  dist_embalses,
  dist_drenaje_doble,
  dist_areas_protegidas,
  dist_lineas_electricas,
  terrain_res,      # elevacion, orientacion, pendiente, sombreado
  r_pobreza,
  r_poblac,
  r_educ
)

names(static_stack) <- c(
  "dist_urbano",
  "dist_vias_primarias",
  "dist_embalses",
  "dist_drenaje_doble",
  "dist_areas_protegidas",
  "dist_lineas_electricas",
  "elevacion",
  "orientacion",
  "pendiente",
  "sombreado",
  "pobreza",
  "poblacion",
  "educacion"
)

static_stack





# 5. Climatologías mensuales de precipitación, tmax, tmin, humedad relativa ----

# Voy a restringir el período a 2001–2023 para que sea consistente con NDVI (ajústalo si quieres otra ventana).

# Fechas
time_prcp <- time(precipitacion)
time_tmax <- time(temp_max)
time_tmin <- time(temp_min)
time_rh   <- time(humedad_rel)

# Ventana temporal (opcional)
start_clim <- as.Date("2001-01-01")
end_clim   <- as.Date("2023-12-31")

prcp_sub <- precipitacion[[time_prcp >= start_clim & time_prcp <= end_clim]]
tmax_sub <- temp_max[[      time_tmax >= start_clim & time_tmax <= end_clim]]
tmin_sub <- temp_min[[      time_tmin >= start_clim & time_tmin <= end_clim]]
rh_sub   <- humedad_rel[[   time_rh   >= start_clim & time_rh   <= end_clim]]

# Índices de mes (1-12)
idx_prcp <- as.integer(format(time(prcp_sub), "%m"))
idx_tmax <- as.integer(format(time(tmax_sub), "%m"))
idx_tmin <- as.integer(format(time(tmin_sub), "%m"))
idx_rh   <- as.integer(format(time(rh_sub),   "%m"))

# Climatología mensual (promedio por mes)
prcp_clim <- tapp(prcp_sub, idx_prcp, fun = mean, na.rm = TRUE)
tmax_clim <- tapp(tmax_sub, idx_tmax, fun = mean, na.rm = TRUE)
tmin_clim <- tapp(tmin_sub, idx_tmin, fun = mean, na.rm = TRUE)
rh_clim   <- tapp(rh_sub,   idx_rh,   fun = mean, na.rm = TRUE)

# Re-muestrear a plantilla
prcp_clim_res <- resample(prcp_clim, template, method = "bilinear")
tmax_clim_res <- resample(tmax_clim, template, method = "bilinear")
tmin_clim_res <- resample(tmin_clim, template, method = "bilinear")
rh_clim_res   <- resample(rh_clim,   template, method = "bilinear")



# 6. Climatología mensual de NDVI (y escalado /10 000) ----
# Nombres: "NDVI_Jan_2001", ..., "NDVI_Dec_2023"
ndvi_names <- names(ndvi_car)

# Extraer "Jan", "Feb", etc.
ndvi_month_abb <- sapply(strsplit(ndvi_names, "_"), `[`, 2)
# Convertir a índice de mes 1-12
idx_ndvi <- match(ndvi_month_abb, month.abb)

# Climatología mensual
ndvi_clim_raw <- tapp(ndvi_car, idx_ndvi, fun = mean, na.rm = TRUE)

# Escalar a [-1,1] aprox, consistente con data_model (NDVI/10000)
ndvi_clim_scaled <- ndvi_clim_raw / 10000

# Re-muestrear a plantilla
ndvi_clim_res <- resample(ndvi_clim_scaled, template, method = "bilinear")




# Asegúrate de que y tiene niveles como c("0","1") o similar
levels(data_model$y)

# rf_fun <- function(df) {
#   # df es un data.frame con columnas numéricas y nombres de predictores
#   preds <- predict(rf_fit, new_data = df, type = "prob")
#   # Columna de probabilidad de la clase "1" (ajusta si tus niveles son distintos)
#   as.numeric(preds$.pred_1)
# }

rf_fun <- function(d) {
  # d llega como matriz/data.frame: filas = celdas, columnas = predictores
  df <- as.data.frame(d)
  
  # usa el workflow ya entrenado
  preds <- predict(rf_fit, new_data = df, type = "prob")
  
  # probabilidad de la clase positiva (ajusta si tu nivel no es "1")
  as.numeric(preds$.pred_1)
}




# Chequeo de dimensiones
static_stack
prcp_clim_res
tmax_clim_res
tmin_clim_res
rh_clim_res
ndvi_clim_res



# Lista donde vamos a guardar los 12 rasters
prob_list <- vector("list", length = 12)

for (m in 1:12) {
  cat("Procesando mes", m, "...\n")
  
  # 1) Extraer capa m de las climatologías
  rh_m    <- rh_clim_res[[m]]
  tmax_m  <- tmax_clim_res[[m]]
  tmin_m  <- tmin_clim_res[[m]]
  prcp_m  <- prcp_clim_res[[m]]
  ndvi_m  <- ndvi_clim_res[[m]]
  
  # 2) Stack completo de predictores para el mes m
  pred_stack_m <- c(
    static_stack,  # distancias, terreno, pobreza/población/educación
    rh_m,
    tmax_m,
    tmin_m,
    prcp_m,
    ndvi_m
  )
  
  # 3) Nombres EXACTOS como en data_model (sin la respuesta "y")
  names(pred_stack_m) <- c(
    "dist_urbano",
    "dist_vias_primarias",
    "dist_embalses",
    "dist_drenaje_doble",
    "dist_areas_protegidas",
    "dist_lineas_electricas",
    "elevacion",
    "orientacion",
    "pendiente",
    "sombreado",
    "pobreza",
    "poblacion",
    "educacion",
    "rh_evt",
    "tmax_evt",
    "tmin_evt",
    "prcp_evt",
    "ndvi_evt"
  )
  
  # 4) Opcional: limitar al área de estudio con dist_urbano
  pred_stack_m <- mask(pred_stack_m, dist_urbano)
  
  # 5) Raster -> data.frame (una fila por celda, una columna por predictor)
  #    na.rm = FALSE para mantener el orden 1: ncell
  df_pred <- as.data.frame(pred_stack_m, na.rm = FALSE)
  
  # 6) Predicción con tu workflow rf_fit (tipo prob)
  preds_tb <- predict(rf_fit, new_data = df_pred, type = "prob")
  
  # Ajusta el nombre de la columna si tu clase positiva no es "1"
  prob_vec <- as.numeric(preds_tb$.pred_1)
  
  # 7) Crear raster de probabilidad y asignar valores
  prob_rast_m <- pred_stack_m[[1]]          # copia geometría (extent, crs, etc.)
  prob_rast_m <- setValues(prob_rast_m, prob_vec)
  names(prob_rast_m) <- paste0("prob_y_m", sprintf("%02d", m))
  
  # 8) Guardar en la lista
  prob_list[[m]] <- prob_rast_m
}

# 9) Stack final de 12 meses
prob_stack <- rast(prob_list)
prob_stack

writeRaster(pred_stack_m, "data/final/predictores_randomforest.tif", overwrite = TRUE)
writeRaster(prob_stack, "data/final/prob_y_climatologia_mensual_4326.tif", overwrite = TRUE)
# plot(prob_stack >0.7)

# prob_stack %>% 
#   terra::mask(jurisdiccion_car) %>% 
#   setNames(month.abb) %>% 
#   plot(col=colorRampPalette(c("darkgreen", "yellow", "red"))(255))



# , main = "Propabilidad de Ocurrencia de Incendios - Modelo RandomForest") 



# 1 = baja, 2 = media-baja, 3 = media-alta, 4 = alta
prob_class <- terra::app(
  prob_stack,
  fun = function(x) {
    cut(
      x,
      breaks = c(0, 0.25, 0.50, 0.75, 1),    # [0–0.25], (0.25–0.5], ...
      include.lowest = TRUE,
      labels = FALSE
    )
  }
)



cols4 <- colorRampPalette(c("darkgreen", "yellow", "red"))(4)

# prob_class %>% 
#   terra::mask(jurisdiccion_car) %>% 
#   setNames(month.abb) %>% 
#   plot(col = cols4)



prob_class_fac <- as.factor(prob_class)

# definir tabla de niveles para todas las capas
lev_tab <- data.frame(
  ID    = 1:4,
  clase = c("Baja",
            "Media-baja",
            "Media-alta",
            "Alta")
)

levels(prob_class_fac) <- lapply(1:nlyr(prob_class_fac), function(i) lev_tab)

# prob_class_fac %>%
#   terra::mask(jurisdiccion_car) %>%
#   setNames(month.abb) %>%
#   plot(col = cols4)











