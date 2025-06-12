# Cargar librerías necesarias
# library(leaflet)
# library(sf)
# library(terra)
# library(RColorBrewer)
# library(viridis)
# library(tidyverse)

# 1. Un vector con todos los nombres de layer
# # geoCAR_layers$layer
# selected_vars <- read_csv("data/geoambiental_car_select.csv")
# descarga_fuente <- FALSE




geocar_selected <- selected_vars %>% left_join(geoCAR_layers)

#1.1. Selecciona el municipio de interes
#sel_municipios <- c("RICAURTE", "NILO", "GUADUAS", "BOJACÁ", "RÁQUIRA")
#sel_municipios <- c("GUADUAS")
write_layer_gpkg <- function(layer, layer_name, layer_reference, dir_data) {
  # 1. Leer
  obj_sf <- layer
  
  # 2. Validar y reparar
  if (any(!st_is_valid(obj_sf))) {
    obj_sf <- st_make_valid(obj_sf)
  }
  
  # 3. Reproyectar
  obj_sf <- st_transform(obj_sf, st_crs(layer_reference))
  
  # 4. Recortar (topológico)
  #  clc_recortado_sf <- st_intersection(obj_sf, jurisdiccion_car)
  
  # 5. Guardar
  st_write(obj_sf,
           paste0(dir_data, layer_name, ".gpkg"),
           delete_layer = FALSE)
  
}



## Descarga 
if(isTRUE(descarga_fuente)){
  
  # 2. Obtener los índices con map_dbl (devuelve numérico) o map_int (entero)
  # id_layers_mapa <- map_int(layer_names, ~ which(geoCAR_layers$layer == .x))
  
  # Descarga mapa geoservicios CAR
  layers_geocar <- map(1:nrow(geocar_selected), 
                       ~load_arcgis_layer(
                         service_url = geocar_selected$url_service[.x], 
                         layer_id = geocar_selected$id[.x],
                         layer_name =  geocar_selected$layer[.x], 
                         epsg = geocar_selected$EPSG[.x]
                       ))
  
  
  
  # reombra y reprojecta
  layers_geocar <- setNames(layers_geocar, geocar_selected$layer) 
  
  # Area de jurisdiccion de la CAR
  layer_reference <- layers_geocar[["Direcciones Regionales"]]
  
  
  names_layer <- geocar_selected$layer %>% str_to_lower() %>%
    str_remove_all("-") %>% str_replace_all(" |  ", replacement = "_") %>% 
    str_replace_all(":", replacement = "_") %>%
    str_replace_all(fixed("."), replacement = "")
  
  layer_name <- map2_chr(geocar_selected$id, names_layer, ~paste(.x, .y, sep = "_"))
  
  geocar_selected$file_name <- paste0(layer_name, ".gpkg")
  
  write_csv(geocar_selected, paste0(dir_geocar, "selected_layers.csv"))
  
  
  map2(layers_geocar, tag, ~write_layer_gpkg(.x, .y, layer_reference, dir_geocar))
  
  
  message(paste0(length(layer_name), " Entidades espaciales guardadas en : ", dir_geocar))
  
  
} else {
  
  files_in_folder <- read_csv(list.files(dir_geocar, full.names = TRUE, pattern = "csv$"))
  
 
  layers_geocar <- map(paste0(dir_geocar, files_in_folder$file_name),
                        st_read) %>% setNames(files_in_folder$layer)
  
  
  message(paste0(nrow(files_in_folder), " Entidades espaciales importadas desde : ", dir_geocar))
  
}




## Puntos criticos 
layer_incendios <- "Riesgos - Registros de eventos"
id_layer_incendios <- which(geoCAR_layers$layer == layer_incendios)
# 
# # Descarga mapa geoservicios CAR
layers_eventos_dgoat <- load_arcgis_layer(service_url = geoCAR_layers$url_service[id_layer_incendios],
                                          layer_id = geoCAR_layers$id[id_layer_incendios],
                                          layer_name =  geoCAR_layers$layer[id_layer_incendios],
                                          epsg = geoCAR_layers$EPSG[id_layer_incendios])
# 








