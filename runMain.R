### Sistema Integral para la Detección, Monitoreo, Análisis y Pronóstico de Incendios en la Cobertura Vegetal
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## mayo 2025

## Packages and dependencies ----
source("requirements.R")

## Crea-verifica estructura de carpetas ----
dir.create("data/")
Map(dir.create, c("data/raw/", "data/interm/", "data/final/"))

## Load Internal functions ----
source("src/funciones_sat_car.R")

source("src/visor_car.R")
geoCAR_layers$layer


## Data Ingestion CAR ----
#sel_municipios <- c("RÁQUIRA")
selected_vars <- read_csv("aux_files/geoambiental_car_select.csv")
descarga_fuente <- FALSE
dir_geocar <- "data/raw/car/" 
source("src/ingest_layer_geocar_all.R")


## Data Ingestion Eventos _ Incendios ----
source("src/ingest_layer_eventos.R")


## Data Ingestion Combustible  Pettinari 2015 ----
# source("src/ingest_layer_combustible.R")


## Data Ingestion Coberturas de la tierra ----
# source("src/ingest_layer_coberturas")

## Data Ingest Satelital Data ----
# source("src/ingest_layer_satelital.R")

## Map plot -  para analisis y Plotter ----
sel_municipios <- c("NILO")
source("src/MAP_incendios_taller.R")
source("src/MAP_incendios_taller_plotter.R")


 ## Preprocess vector data
# source("src/preprocess_vector_data.R")

## Rasterize layer to model
# source("src/rasterize_layers.R")
