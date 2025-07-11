## Ingest_ data IDEAM - Cubo datos

files_ideam <- list.files("data/raw/ideam/meteorologicos/", full.names = TRUE, recursive = TRUE, pattern = "*.nc$")

files_ideam_mensual <- files_ideam %>% str_subset("_mes_")


rast(files_ideam_mensual[1])

jurisdiccion_car

# guardar en "data/interm/meteorologicos/"
# 
# 
# names --> ideam_*_mensual (* = precipitacion, tmax, tmed, tmin, humd)

# 2. Prepara la lista de archivos y un vector con los sufijos deseados
files <- files_ideam_mensual
sufijos <- c("precipitacion", "tmax", "tmed", "tmin", "humd")
names(files) <- sufijos

# 3. Crea la carpeta de salida si no existe
out_dir <- "data/interm/meteorologicos"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 4. Función para procesar un único archivo
procesa_ideam <- function(ruta_nc, sufijo, jurisdiccion) {
  # lee todo el stack
  r <- rast(ruta_nc)
  
  # recorta al extent de la jurisdicción
  r_c <- crop(r, jurisdiccion)
  
  # enmascara para dejar sólo los píxeles dentro de la CAR
  r_m <- mask(r_c, jurisdiccion)
  
  # arma nombre de salida (GeoTIFF)
  out_file <- file.path(out_dir, paste0("ideam_", sufijo, "_mensual.tif"))
  
  # escribe (sobrescribe si existe)
  writeRaster(r_m, out_file, overwrite = TRUE)
}

# 5. Itera sobre todos los archivos
imap(files, ~ procesa_ideam(.x, .y, jurisdiccion_car))



rast("data/interm/meteorologicos/ideam_precipitacion_mensual.tif")
