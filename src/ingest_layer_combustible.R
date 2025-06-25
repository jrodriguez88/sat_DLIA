# library(terra)
# library(tidyverse)
# library(sf)


jurisdiccion_car <- vect("data/raw/car/11_direcciones_regionales.gpkg")


jurisdiccion_car_4326 <- st_transform(st_as_sf(jurisdiccion_car), crs = 4326) %>%
  group_by(Direccion) %>%
  summarise(geometry = st_union(geometry))


buffer_jurisdiccion <- st_buffer(st_union(jurisdiccion_car_4326), dist = 20000)

# plot(buffer_jurisdiccion)
# lines(jurisdiccion_car_4326)

# Tile Suramerica
tile3 <- "data/raw/pettinari2015/Global_fuelbeds_map_Tile3.tif"

# 1. Leer raster original
fuel_bed <- rast(tile3)

# 2. Reproyectar usando vecino más cercano para datos categóricos
# fuel_bed_proj <- project(fuel_bed,
#                          crs(jurisdiccion_car),
#                          method = "near")

# 3. Recortar y enmascarar a tu vector de jurisdicción
fuel_bed_car <- crop(fuel_bed, jurisdiccion_car_4326, mask = TRUE)

# 4. Comprobar que tus valores están intactos
# unique(values(fuel_bed_car))

# Corte Jurisdiccion CAR

# fuel_bed_car <- project(fuel_bed, crs = crs(jurisdiccion_car)) %>% 
#   crop(jurisdiccion_car, mask = T)


writeRaster(fuel_bed_car, "data/interm/fuel_bed_car_ID.tif", overwrite = TRUE)

# plot(fuel_bed_car)
# plot(as.factor(fuel_bed_car))
# freq(fuel_bed_car)
# terra::summary(fuel_bed_car)


fuel_bed_parameters <- read_excel("data/raw/pettinari2015/Global_fuelbeds_parameters_v1.2.xlsx", 
                                  sheet = "Fuelbeds_metric")


# fuel_bed_parameters$Biome %>% unique()


#───────────────────────────────────────────────────────────
# Función genérica para extraer cualquier variable como raster
#───────────────────────────────────────────────────────────
# var_name:   nombre exacto de la columna en fuel_bed_parameters
# params_df:  fuel_bed_parameters (tibble con JOIN_VALUE + variables)
# rast_join:  SpatRaster con valores de JOIN_VALUE (fuel_bed_car)
#───────────────────────────────────────────────────────────
# generate_var_raster <- function(var_name, params_df, rast_join) {
#   # 1) extraemos solo JOIN_VALUE y la variable deseada
#   lookup <- params_df %>%
#     select(JOIN_VALUE, !!sym(var_name)) %>%
#     distinct()
#   
#   # 2) ¿es variable numérica o carácter?  
#   is_num <- is.numeric(lookup[[var_name]])
#   
#   if (is_num) {
#     # Para variables numéricas, uso terra::subst()
#     out <- terra::subst(
#       x    = rast_join,
#       from = lookup$JOIN_VALUE,
#       to   = lookup[[var_name]],
#       others = NA
#     )
#     names(out) <- var_name
#     
#   } else {
#     # Para variables de texto, convierto el raster a factor y asigno niveles
#     out <- as.factor(rast_join)
#     # terra guarda los niveles en levels(out)[[1]] como data.frame(ID, Category)
#     rat <- data.frame(ID = lookup$JOIN_VALUE,
#                       Category = lookup[[var_name]])
#     levels(out)[[1]] <- rat
#     names(out) <- var_name
#   }
#   
#   return(out)
# }



generate_var_raster <- function(var_name, params_df, rast_join) {
  # 1) Extrae la tabla de lookup
  lookup <- params_df %>%
    select(JOIN_VALUE, !!sym(var_name)) %>%
    distinct()
  
  # 2) Genera el raster
  is_num <- is.numeric(lookup[[var_name]])
  if (is_num) {
    out <- terra::subst(
      x      = rast_join,
      from   = lookup$JOIN_VALUE,
      to     = lookup[[var_name]],
      others = NA
    )
    names(out) <- var_name
    
  } else {
    # crea un raster-factor
    out <- as.factor(rast_join)
    rat <- data.frame(
      ID       = lookup$JOIN_VALUE,
      Category = lookup[[var_name]],
      stringsAsFactors = FALSE
    )
    levels(out)[[1]] <- rat
    names(out) <- var_name
  }
  
  # 3) PODA: elimina niveles que no aparecen en ningún píxel
  #   a) identifica los JOIN_VALUE presentes en el área
  present_ids <- terra::freq(rast_join)[,"value"]
  #   b) para rasters numéricos no es necesario; para factores:
  if (!is_num) {
    rat2 <- levels(out)[[1]] %>%
      filter(ID %in% present_ids)
    levels(out)[[1]] <- rat2
  }
  
  return(out)
}



# tu raster base
# rast_join <- fuel_bed_car

# tu tabla de parámetros
params_df <- fuel_bed_parameters
# par(mfrow = c(1, 3))
# 
# # 1) Tree Cover (%) → raster numérico
# rast_tree  <- generate_var_raster("Tree Cover (%)", params_df, fuel_bed_car)
# plot(rast_tree, main="Tree Cover (%)")
# 
# # 2) Biome → raster categórico
# rast_biome <- generate_var_raster("Biome", params_df, fuel_bed_car)
# plot(rast_biome, main="Biome", type="classes")
# 
# # 3) LandCover → raster categórico
# rast_lc    <- generate_var_raster("LandCover", params_df, fuel_bed_car)
# plot(rast_lc, main="LandCover", type="classes")
# 
# 
# rast_lc    <- generate_var_raster("LandCover", params_df, fuel_bed_car)
# plot(rast_lc, main="LandCover", type="classes")
# 
# names(params_df)

raster_combust <- map(names(params_df)[c(2, 4:67)], ~generate_var_raster(.x, params_df, fuel_bed_car))

raster_combust <- setNames(raster_combust, names(params_df)[c(2, 4:67)])

all_rast <- rast(raster_combust)
names(all_rast) <- names(params_df)[c(2, 4:67)]

writeRaster(all_rast, filename = "data/interm/fuel_bed_car_all.tif", datatype = "FLT4S", overwrite = TRUE)


# saveRDS(rast(raster_combust), "data/interm/fuelbeds.rds")
# r2 <- readRDS("data/interm/fuelbeds.rds")



dir.create("data/interm/combustible")
names_rast_comb <- paste0(str_replace_all(names(params_df)[c(2, 4:67)], " ", "_"), ".tif") %>% 
  str_replace_all("#", "No") %>% str_replace_all("/", "_per_")
map2(.x = raster_combust, .y = names_rast_comb,
     ~writeRaster(.x, filename = paste0("data/interm/combustible/", .y), overwrite = T))


par(mfrow = c(2, 2))
map2(raster_combust, names(params_df)[c(2, 4:67)], ~plot(.x, main=.y, type="classes"))
# dev.off()


# test <- list.files("data/interm/combustible/", pattern = ".tif$", full.names = TRUE)[1] %>%
#   map(rast)
# 
# 
# 
# test2 <- rast("data/interm/combustible/fuel_bed_car_all.tif")
