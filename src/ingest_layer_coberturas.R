## Corine Land Cover & Ecosistemas - IDEAM ----

# library(terra)
# library(sf)
# library(tmap)

# dir.create("data/raw/ideam/coberturas")
dir.create(dir_coberturas)

# # 2. Obtener los índices con map_dbl (devuelve numérico) o map_int (entero)
# id_layers_mapa <- which(geoCAR_layers$layer == "Direcciones Regionales")
# 
# # Descarga mapa geoservicios CAR
# layers_mapa <- map(id_layers_mapa, ~load_arcgis_layer(service_url = geoCAR_layers$url_service[.x], 
#                                                       layer_id = geoCAR_layers$id[.x],
#                                                       layer_name =  geoCAR_layers$layer[.x], 
#                                                       epsg = geoCAR_layers$EPSG[.x]
# ))

# jurisdiccion_car <- layers_mapa[[1]]


## Carga de datos ----


dir_coberturas_raw <- "data/raw/ideam/coberturas/"


if(isTRUE(crop_from_source)){
  
  
  ## IDEAM _ CLC 
  
  path_list <- list(
    
    path_shp_clc_2000_2002 = paste0(dir_coberturas_raw, "/Cobertura_tierra_2000_2002/shape/cobertura_tierra2000_2002V2.shp"),
    path_shp_clc_2005_2009 = paste0(dir_coberturas_raw, "/Cobertura_tierra_2005_2009/shape/cobertura_tierra2005_2009.shp"),
    path_shp_clc_2010_2012 = paste0(dir_coberturas_raw, "/Cobertura_tierra_2010_2012/SHP/Cobertura_tierra_2010_2012.shp"),
    path_shp_clc_2018 = paste0(dir_coberturas_raw, "/COBERTURAS CORINE 2018/shape coberturas 2018/cobertura_tierra_clc_2018.shp"),
    path_shp_clc_2020 = paste0(dir_coberturas_raw, "/Cobertura_de_la_tierra_100K_Periodo_2020_limite_administrativo/Shape limite Ambiental/e_cobertura_tierra_2020_amb.shp")
    
  )
  
  # poly_shp <- caqueta_shp
  
  ## Elegir uno 
  
  # clc_data <- vect(path_shp_clc_2000_2002)
  # clc_data <- vect(path_shp_clc_2005_2009)
  # clc_data <- vect(path_shp_clc_2010_2012)  # clc_data <- vect(path_shp_clc_2018)
  # clc_data <- vect(path_shp_clc_2020)
  # 
  # for(path in path_list){
  #   
  #   
  #   tag_periodo <- names(path) %>% str_remove_all("path_shp_clc_")
  #   
  #   
  #   # 1. Leer
  #   clc_sf <- st_read(path, quiet = TRUE)
  #   
  #   # 2. Validar y reparar
  #   if (any(!st_is_valid(clc_sf))) {
  #     clc_sf <- st_make_valid(clc_sf)
  #   }
  #   
  #   # 3. Reproyectar
  #   clc_sf <- st_transform(clc_sf, st_crs(jurisdiccion_car))
  #   
  #   # 4. Recortar (topológico)
  #   clc_recortado_sf <- st_intersection(clc_sf, jurisdiccion_car)
  #   
  #   # 5. Guardar
  #   st_write(clc_recortado_sf,
  #            paste0(dir_coberturas, "/clc_car_ideam", tag_periodo, ".gpkg"),
  #            delete_layer = FALSE)
  #   
  #   # Liberar Mem
  #   gc()
  #   
  #   
  # }
  
  
  tag_periodo <- names(path_list) %>% str_remove_all("path_shp_clc_")


  lee_recorta_clc <- function(path, tag_periodo) {
    # 1. Leer
    clc_sf <- st_read(path, quiet = TRUE)

    # 2. Validar y reparar
    if (any(!st_is_valid(clc_sf))) {
      clc_sf <- st_make_valid(clc_sf)
    }

    # 3. Reproyectar
    clc_sf <- st_transform(clc_sf, st_crs(jurisdiccion_car))

    # 4. Recortar (topológico)
    clc_recortado_sf <- st_intersection(clc_sf, jurisdiccion_car)

    # 5. Guardar
    st_write(clc_recortado_sf,
             paste0(dir_coberturas, "/coberturas_car_clc_ideam_", tag_periodo, ".gpkg"),
             delete_layer = FALSE)

  }

  map2(path_list, tag_periodo, lee_recorta_clc)
  
  gc()
  
  
  ## IDEAM - ECOSISTEMAS
  
  # "D:\00_DEVELOPER\sat_DLIA\data\raw\ideam\Mapa_Ecosistemas_Continentales_Costeros_Marinos_100K_2024\SHAPE\e_eccmc_100K_2024.shp"
  eco_sf1 <- st_read("data/raw/car/1_ecosistemas__ideam_año_2017.gpkg", quiet = TRUE)
  
  # 2. Validar y reparar
  if (any(!st_is_valid(eco_sf1))) {
    eco_sf1 <- st_make_valid(eco_sf1)
  }
  
  # 3. Reproyectar
  eco_sf1 <- st_transform(eco_sf1, st_crs(jurisdiccion_car))
  
  # 4. Recortar (topológico)
  eco_recortado_sf1 <- st_intersection(eco_sf1, jurisdiccion_car)
  
  # 5. Guardar
  st_write(eco_recortado_sf1,
           paste0(dir_coberturas, "coberturas_car_ecosistemas_ideam_2017", ".gpkg"),
           delete_layer = FALSE)
  
  
  
  
  # "D:\00_DEVELOPER\sat_DLIA\data\raw\ideam\Mapa_Ecosistemas_Continentales_Costeros_Marinos_100K_2024\SHAPE\e_eccmc_100K_2024.shp"
  eco_sf <- st_read("data/raw/ideam/Mapa_Ecosistemas_Continentales_Costeros_Marinos_100K_2024/SHAPE/e_eccmc_100K_2024.shp", quiet = TRUE)
  
  # 2. Validar y reparar
  if (any(!st_is_valid(eco_sf))) {
    eco_sf <- st_make_valid(eco_sf)
  }
  
  # 3. Reproyectar
  eco_sf <- st_transform(eco_sf, st_crs(jurisdiccion_car))
  
  # 4. Recortar (topológico)
  eco_recortado_sf <- st_intersection(eco_sf, jurisdiccion_car)
  
  # 5. Guardar
  st_write(eco_recortado_sf,
           paste0(dir_coberturas, "coberturas_car_ecosistemas_ideam_2024", ".gpkg"),
           delete_layer = FALSE)
  
  
  
  
} else {
  
  ## lee fuente 
  

  list_shp_clc_car <- list.files(dir_coberturas, full.names = T, pattern = "clc_ideam") %>% str_subset(".tif$", negate = T)
  list_shp_ecosistemas_car <- list.files(dir_coberturas, full.names = T, pattern = "ecosistemas_ideam")
  


}













