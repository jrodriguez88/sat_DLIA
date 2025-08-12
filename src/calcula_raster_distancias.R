# -------------------------------------------------------------------------
#  crea_raster_distance_v2
#  • Genera un ráster de distancias (m) desde un objeto vectorial.
#  • Recorta y enmascara con un AOI opcional.
#  • Devuelve el ráster en el CRS original del AOI (si se proporciona).
# -------------------------------------------------------------------------
#  Argumentos
#    raster_base : SpatRaster (cualquier CRS)
#    spatial_obj : sf / SpatVector  (puntos, líneas o polígonos)
#    aoi         : sf / SpatVector  (polígono(s) de recorte).  Opcional
#    crs_proj    : CRS intermedio (proyección métrica). Por defecto EPSG:9377
#    resol       : resolución de salida en metros (antes de reproyectar al AOI)
#    method      : método de remuestreo para la proyección del ráster base
#


raster_base <- rast_terrain[[1]]

crea_raster_distance <- function(raster_base,
                                 spatial_obj,
                                 aoi        = NULL,
                                 crs_proj   = "EPSG:9377",
                                 resol      = 100,
                                 method     = "bilinear") {
  
  # ── 0. Paquetes ─────────────────────────────────────────────────────────
  if (!requireNamespace("terra", quietly = TRUE) ||
      !requireNamespace("sf",    quietly = TRUE)) {
    stop("Instala los paquetes 'terra' y 'sf' antes de continuar.")
  }
  library(terra)
  library(sf)
  
  # ── 1. Reproyectar ráster base a crs_proj y resolución deseada ─────────
  r_proj <- terra::project(
    raster_base,
    crs_proj,
    res    = resol,
    align  = TRUE,
    method = method
  )
  
  # ── 2. Reproyectar y convertir el objeto vectorial ─────────────────────
  v_proj <- sf::st_as_sf(spatial_obj)        |> sf::st_geometry() |>
    sf::st_transform(crs_proj)      |>
    terra::vect()
  
  # ── 3. Rasterizar el vector sobre r_proj ───────────────────────────────
  feat_rast <- terra::rasterize(
    v_proj,
    r_proj,
    field      = 1,
    touches    = TRUE,
    background = NA
  )
  
  # ── 4. Distancias euclidianas (m) ───────────────────────────────────────
  dist_rast <- terra::distance(feat_rast)
  
  # ── 5. Recorte y máscara con AOI (si existe) ───────────────────────────
  if (!is.null(aoi)) {
    aoi_sf      <- sf::st_as_sf(aoi)
    aoi_crs     <- sf::st_crs(aoi_sf)    # CRS original a conservar
    aoi_proj    <- sf::st_transform(aoi_sf, crs_proj) |>
      terra::vect()
    
    dist_rast   <- terra::crop(dist_rast,  aoi_proj)
    dist_rast   <- terra::mask(dist_rast,  aoi_proj)
    
    # ─ Reproyectar de vuelta al CRS del AOI ─
    #   Mantiene la celda ~100 m en la proyección intermedia; la resolución
    #   angular resultante variará ligeramente con la latitud.
    dist_rast   <- terra::project(dist_rast, aoi_crs$wkt, method = "bilinear")
  }
  
  return(dist_rast)
}


# Distancia a Casco Urbano

dist_urbano <- crea_raster_distance(
  raster_base = raster_base,   # tu ráster de elevación
  spatial_obj = centro_poblados,   # cascos urbanos
  aoi         = jurisdiccion_car,           # área de interés
  resol       = 100            # 100 m
)

plot(dist_urbano, main = "Distancia (m) al casco urbano")
lines(centro_poblados)


# Distancia a drenaje doble

dist_drenaje_doble <- crea_raster_distance(
  raster_base = raster_base,   # tu ráster de elevación
  spatial_obj = drenaje_doble,   # cascos urbanos
  aoi         = jurisdiccion_car,           # área de interés
  resol       = 100            # 100 m
)


plot(dist_drenaje_doble, main = "Distancia (m) a drenaje doble")
lines(drenaje_doble)
# lines(embalses)
# lines(jurisdiccion_car)

# Distancia a Areas protegidas
dist_areas_protegidas <- crea_raster_distance(
  raster_base = raster_base,   # tu ráster de elevación
  spatial_obj = areas_protegidas,   # cascos urbanos
  aoi         = jurisdiccion_car,           # área de interés
  resol       = 100            # 100 m
)


plot(dist_areas_protegidas, main = "Distancia (m) a Areas protegidas")
lines(areas_protegidas)


## Distancia lineas electricas

dist_lineas_electricas <- crea_raster_distance(
  raster_base = raster_base,   # tu ráster de elevación
  spatial_obj = lineas_electricas,   # cascos urbanos
  aoi         = jurisdiccion_car,           # área de interés
  resol       = 100            # 100 m
)


plot(dist_lineas_electricas, main = "Distancia (m) a Lineas Electricas")
lines(lineas_electricas)



## Distancia vias primarias

vias %>% dplyr::filter(TIPO_VIA == 1) %>% st_geometry() %>% plot
lines(centro_poblados, col = "red")
lines(jurisdiccion_car, col = "brown")



dist_vias_primarias <- crea_raster_distance(
  raster_base = raster_base,   # tu ráster de elevación
  spatial_obj = vias %>% dplyr::filter(TIPO_VIA == 1),   # cascos urbanos
  aoi         = jurisdiccion_car,           # área de interés
  resol       = 100            # 100 m
)


plot(dist_vias_primarias, main = "Distancia (m) a Vias primarias")
lines(vias %>% dplyr::filter(TIPO_VIA == 3))
lines(jurisdiccion_car, col = "red")


## Distancia Embalses


dist_embalses <- crea_raster_distance(
  raster_base = raster_base,   # tu ráster de elevación
  spatial_obj = embalses,
  aoi         = jurisdiccion_car,           # área de interés
  resol       = 100            # 100 m
)


plot(dist_embalses, main = "Distancia (m) a Embalses")
lines(embalses)


raster_distancias_car <- list(dist_urbano = dist_urbano, dist_areas_protegidas = dist_areas_protegidas,
     dist_drenaje_doble = dist_drenaje_doble, dist_vias_primarias = dist_vias_primarias, 
     dist_embalses = dist_embalses, dist_lineas_electricas = dist_lineas_electricas) %>% 
  rast() 

plot(raster_distancias_car)

writeRaster(raster_distancias_car, filename = "data/final/raster_distancias_car.tif", overwrite = TRUE)


