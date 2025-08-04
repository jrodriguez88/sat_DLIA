## Ingest_ data IDEAM - Cubo datos

# http://archivo.ideam.gov.co/web/tiempo-y-clima/clima



files_ideam <- list.files("data/raw/ideam/meteorologicos/", full.names = TRUE, recursive = TRUE, pattern = "*.nc$")

files_ideam_mensual <- files_ideam %>% str_subset("_mes_")


# rast(files_ideam_mensual[1])

# jurisdiccion_car

# guardar en "data/interm/meteorologicos/"
# 
# 
# names --> ideam_*_mensual (* = precipitacion, tmax, tmed, tmin, humd)

# 2. Prepara la lista de archivos y un vector con los sufijos deseados
files <- files_ideam_mensual
sufijos <- c("precipitacion", "tmax", "tmed", "tmin", "humd")
names(files) <- sufijos

# 3. Crea la carpeta de salida si no existe

# dir_meteo <- "data/interm/meteorologicos"
if (!dir.exists(dir_meteo)) dir.create(dir_meteo, recursive = TRUE)
# 4. Función para procesar un único archivo
procesa_ideam <- function(ruta_nc, sufijo, aoi) {
  # lee todo el stack
  r <- rast(ruta_nc)
  
  # recorta al extent de la jurisdicción
  r_c <- crop(r, aoi)
  
  # enmascara para dejar sólo los píxeles dentro de la CAR
  r_m <- mask(r_c, jurisdiccion)
  
  # arma nombre de salida (GeoTIFF)
  out_file <- file.path(out_dir, paste0("ideam_", sufijo, "_mensual.tif"))
  
  # escribe (sobrescribe si existe)
  writeRaster(r_m, out_file, overwrite = TRUE)
}

# 5. Itera sobre todos los archivos
imap(files, ~ procesa_ideam(.x, .y, jurisdiccion_car))


precipitacion <- rast("data/interm/meteorologicos/ideam_precipitacion_mensual.tif")
temp_max <- rast("data/interm/meteorologicos/ideam_tmax_mensual.tif")
temp_min <- rast("data/interm/meteorologicos/ideam_tmin_mensual.tif")
humedad_rel <- rast("data/interm/meteorologicos/ideam_humd_mensual.tif")
# boxplot(rast("data/interm/meteorologicos/ideam_precipitacion_mensual.tif")[[150]])

# precipitacion[[516]]
# plot(precipitacion[[516]])
# 
# plot(humedad_rel[[516]])
# 
# 
# plot(humedad_rel[[time(humedad_rel) %>% str_detect("2019")]])


# 
# global(rast("data/interm/meteorologicos/ideam_precipitacion_mensual.tif"), mean, na.rm = T)

