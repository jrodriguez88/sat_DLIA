### Sistema Integral para la Detección, Monitoreo, Análisis y Pronóstico de Incendios en la Cobertura Vegetal
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## mayo 2025


# DATA INGESTION WORKFLOW
# Contiene el flujo de trabajo de ingestion de diferentes fuentes de datos requeridos para el desarrollo del sistema



## Packages and dependencies ----
# Carga librerias necesarias para las tareas del workflow, 
source("requirements.R")

## Crea-verifica estructura de carpetas ----
# Asegura la creacion de la estructura base de la carpeta "data"y subcarpetas con distintas fases de datos 
dir.create("data/")
Map(dir.create, c("data/raw/", "data/interm/", "data/final/"))

## Load Internal functions ----
# Carga conjunto de funciones necesarias para el funcionamiento y procesamiento del sistema
source("src/funciones_sat_car.R")


## Data Ingestion Geoservicios CAR ----
# Busca integrar la informacion disponible en los geoservicios de la Corporacion que actualmente estan administrados por la DRN

### WebScraping sobre geoservicios de la CAR ----
# Busca que servicios disponibles en los geoportales de la CAR. Identifica los layer de cada servicio y su metadata basica
source("src/visor_car.R")
geoCAR_layers$layer

# sel_municipios <- c("RÁQUIRA")
selected_vars <- read_csv("aux_files/geoambiental_car_select.csv")
descarga_fuente <- FALSE
dir_geocar <- "data/raw/car/" 
source("src/ingest_layer_geocar_all.R")
print(files_in_geocar)

### Seleccion de capas base ----
# Filtra y selecciona capas de informacion base de limites y entidades territoriales y administrativos
dir_base <- "data/interm/base"
source("src/ingest_capas_base.R")
mapa_test_base


## Ingest data Infraestructura ----
# https://geo.upme.gov.co/server/rest/services/Capas_EnergiaElectrica/Sistema_transmision_lineas_construidas/FeatureServer/17

dir_infraestructura <- "data/interm/infraestructura/"
source("src/ingest_layer_infraestructura.R")
plot(jurisdiccion_car["Direccion"] %>% vect())
lines(lineas_recortado_sf , col = "red")


## Data Ingestion Eventos - Incendios ----
# Se cargan datos de monitoreo de y registro de eventos/ ocurrencia de incendios de cobertura vegetal
# Datos provenientes de DGOAT-CAR, UNGRD y MODIS MCD64A1
dir_eventos <- "data/interm/eventos_ocurrencia/"
source("src/ingest_layer_eventos.R")
naniar::miss_var_summary(test_data_events)
naniar::vis_miss(ungroup(test_data_events))
naniar::miss_var_table(ungroup(test_data_events))
naniar::gg_miss_fct(ungroup(test_data_events) %>% 
                      dplyr::select(-year, -contains("n_")), 
            fct = Municipio)
grafico_numero_eventos
grafico_area_eventos


## Data Ingestion Combustible  Pettinari 2015 ----
# Importa y genera rasters de combustible de la base de datos de pettinari 2015 
# https://doi.pangaea.de/10.1594/PANGAEA.849808
dir_combustible <- "data/interm/combustible/"
source("src/ingest_layer_combustible.R")
par(mfrow = c(2, 2))
map2(raster_combust, names(params_df)[c(2, 4:67)], ~plot(.x, main=.y, type="classes"))
print(names(params_df))

## Data Ingestion Coberturas de la tierra ----
# Importa los datos de coberturas de uso del suelo. CLC - IDEAM y Ecosistemas - IDEAM
# https://experience.arcgis.com/experience/568ddab184334f6b81a04d2fe9aac262/page/Centro-Descarga-Geoportal
dir_coberturas <- "data/interm/coberturas/"
crop_from_source <- FALSE # If TRUE --> RAM minima = 64gb (mensaje advertencia para procesar)
source("src/ingest_layer_coberturas.R")
raster_clc_car <- map(.x = list_shp_clc_car, ~rasterize_clc(.x, out_path = dir_coberturas, clc_level = 2, resol = 250))
tags_car <- basename(list_shp_clc_car) %>% str_remove_all("clc_|.shp") 
par(mfrow = c(1, 5))
map2(raster_clc_car, tags_car, ~plot_raster_clc(.x, clc_level = 2, tag = .y))
par(mfrow = c(1, 1))


## Data Ingest Satelital Data ----
# Importa datos provenientes de fuentes satelitales como NASADEM, MODIS
# dir_satelital <- "data/interm/satelital/"

### Terrain from NASADEM ---- 
# GEE usado para preprocesamiento, calculo con algoritmos de terreno () y descarga de recorte 
# https://developers.google.com/earth-engine/datasets/catalog/NASA_NASADEM_HGT_001?hl=es-419
# https://developers.google.com/earth-engine/apidocs/ee-terrain-products
# source("src/ingest_layer_satelital.R")
dir_terreno <- "data/interm/terreno/"
source("src/ingest_layer_terreno.R")
plot(rast(rast_terrain))


### NDVI from MODIS ----
# GEE usado para descarga, corte y preprocesamiento
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13A1?hl=es-419
dir_vegetacion_index <- "data/interm/veg_index"
source("ingest_layer_ndvi.R")
plot(rast_ndvi)


### Informacion Meteorologica de IDEAM
dir_meteo <- "data/interm/meteorologicos/"
source("src/ingest_layer_meteo.R")
plot(humedad_rel[[time(humedad_rel) %>% str_detect("2019")]])





## Ingest Socioeconomic Data
# Integra datos de indicadores y variables socioeconomicas por municipio. Fuente DANE, 
dir_socioeconomic <- "data/interm/socioeconomica/"
source("src/ingest_layer_socioeconomica.R")
plot(socioeconomic_data_spatial[8:13])
# plot(socioeconomic_data_spatial[8])


## Map plot - Mapas para analisis y Plotter ----
# Scripts para contruir mapas dinamicos par analisis de teritorio o municipio
# Crea mapas en tamaño de impresion A0

sel_municipios <- c("NILO")
source("src/MAP_incendios_taller.R")
source("src/MAP_incendios_taller_plotter.R")


 ## Preprocess vector data
# source("src/preprocess_vector_data.R")

## Rasterize layer to model
# source("src/rasterize_layers.R")


## Siguientes pasos :: Extraccion de datos por puntos de ocurrencia de incendios 
# Evaluar modelos de respuesta presencia ausencia, Maxent, RF, etc 




