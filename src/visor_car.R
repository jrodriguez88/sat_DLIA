# 1. Instala y carga los paquetes necesarios
#install.packages(c("httr", "jsonlite", "sf", "geojsonsf"))
# library(httr)
# library(jsonlite)
# library(sf)
# library(geojsonsf)
#library(terra)

# 2. Define la URL del REST y parámetros comunes
root_url <- "https://sig.car.gov.co/arcgis/rest/services/"
# Si el certificado es self-signed, en httr GET puedes añadir config(ssl_verifypeer = FALSE)

#get_metadata("https://sig.car.gov.co/arcgis/rest/services/VISOR/Aire/FeatureServer")$layers

## load geoservi
geoservices_car <- GET(paste0(root_url, "VISOR?f=json")) %>% 
  content(as = "text", encoding = "UTF-8") %>% fromJSON()

geoCAR <- geoservices_car$services %>% 
  filter(type == "FeatureServer") %>%
  mutate(url_service = paste0(root_url, name, "/FeatureServer")) %>%
  mutate(metadata = map(url_service, get_metadata)) %>% # Extrae la lista de capas
  mutate(layers = map(metadata,
                      ~.x$layers %>% 
                        dplyr::select(id, layer = name, geometryType)),
         descripcion = map_chr(metadata, ~.x$description),
         service_ID = map_chr(metadata, ~.x$serviceItemId),
         EPSG = map(metadata, ~.x$spatialReference$latestWkid),
         EPSG = map_depth(EPSG, 1, ~ifelse(is.null(.x), 9377, .x))) %>% 
  unnest(EPSG)

print(geoCAR)

#write_csv(dplyr::select(geoCAR, -c(metadata, layers)), file = "geoCAR.csv")

# Unlist layer features
geoCAR_layers <- geoCAR %>% dplyr::select(-c(metadata, type)) %>% 
  unnest(cols = layers) %>% 
  select(name, url_service, id, layer , EPSG, geometryType) 

#write_csv(geoCAR_layers, file = "geoCAR_layers.csv")

# id <- 186
# 
# test <- load_arcgis_layer(service_url = geoCAR_layers$url_service[id],
#                           layer_id = geoCAR_layers$id[id],
#                           layer_name =  geoCAR_layers$layer[id],
#                           epsg = geoCAR_layers$EPSG[id]
#                           )
# 
# names(test)
# ggplot() +
# #  geom_sf(data = jurisdiccion_car, aes(fill = Direccion)) + 
#   geom_sf(data = test, alpha = 0.5, aes(fill = GRAD_AME))
# 
# 
# 
# test <- load_arcgis_layer(service_url = geoCAR_layers$url_service[32], 
#                           layer_id = geoCAR_layers$id[32],
#                           layer_name =  geoCAR_layers$layer[32], 
#                           epsg = geoCAR_layers$EPSG[32]
# )
# 
# 
# 
# id <- 115
# test <- load_arcgis_layer(service_url = geoCAR_layers$url_service[115], 
#                           layer_id = geoCAR_layers$id[115],
#                           layer_name =  geoCAR_layers$layer[115], 
#                           epsg = geoCAR_layers$EPSG[115]
# ) 
# 
# 
# write_sf(test, "data/coberturas_car-2016.gpkg")


