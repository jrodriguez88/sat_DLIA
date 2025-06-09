# Cargar librerías necesarias
# library(leaflet)
# library(sf)
# library(terra)
# library(RColorBrewer)
# library(viridis)
# library(tidyverse)

# 1. Un vector con todos los nombres de layer
geoCAR_layers$layer

layer_names <- c(
  "Límite Departamental",
  "Municipios",
  "Direcciones Regionales",
  "Áreas Naturales Protegidas",
  "Ecosistemas - IDEAM año 2017",
  "Veredas",
  "Drenaje Doble",
  "Drenaje sencillo",
  "Embalse",
  "Vía",
  "Cuencas CAR",
  "Centro poblado DANE",
  "Riesgos - Registros de eventos"
  
)


#1.1. Selecciona el municipio de interes
sel_municipios <- c("RICAURTE", "NILO", "GUADUAS", "BOJACÁ", "RÁQUIRA")
sel_municipios <- c("GUADUAS")

## cabeceras
## cuerpos hidricos
## 

# 2. Obtener los índices con map_dbl (devuelve numérico) o map_int (entero)
id_layers_mapa <- map_int(layer_names, ~ which(geoCAR_layers$layer == .x))

# Descarga mapa geoservicios CAR
layers_mapa <- map(id_layers_mapa, ~load_arcgis_layer(service_url = geoCAR_layers$url_service[.x], 
                                                      layer_id = geoCAR_layers$id[.x],
                                                      layer_name =  geoCAR_layers$layer[.x], 
                                                      epsg = geoCAR_layers$EPSG[.x]
))

# reombra y reprojecta
layers_mapa <- setNames(layers_mapa, layer_names) %>% map(~st_transform(.x, crs = 4326))
names(layers_mapa)
layers_mapa %>% map(names)

# Area de jurisdiccion de la CAR
jurisdiccion_car <- layers_mapa[["Direcciones Regionales"]]

layers_mapa[["Ecosistemas - IDEAM año 2017"]]$COBERTURA %>% unique()

municipios_crop <- layers_mapa[["Municipios"]] %>% filter(Municipio %in% sel_municipios) 

DEM <- geodata::elevation_30s(country = "COL", path=tempdir())
car_dem <- crop(project(DEM, crs(jurisdiccion_car)), jurisdiccion_car, mask = T)

municipio_dem <-  crop(project(DEM, crs(municipios_crop)), municipios_crop, mask = T)

plot(car_dem)
plot(municipio_dem)

layers_mapa <- layers_mapa %>% 
  map(~st_make_valid(.x) %>% st_intersection(municipios_crop["Municipio"]))



## Layers del mapa

#3 Eventos de incendios, ubicacion 

eventos_criticos <- cleaning_puntos_criticos(
  layers_mapa[["Riesgos - Registros de eventos"]], 
  clase = "Incendio Forestal")


municipio <- layers_mapa[["Municipios"]]
centro_poblado <- layers_mapa[["Centro poblado DANE"]]
veredas <- layers_mapa[["Veredas"]]
areas_protegidas <- layers_mapa[["Áreas Naturales Protegidas"]]
cuencas <- layers_mapa[["Veredas"]]
ecosistemas <- layers_mapa[["Ecosistemas - IDEAM año 2017"]]

rios <- layers_mapa[["Drenaje Doble"]]
quebradas <- layers_mapa[["Drenaje sencillo"]]
embalses <- layers_mapa[["Embalse"]]
vias <- layers_mapa[["Vía"]] %>% filter(TIPO_VIA  %in% c(1:4))