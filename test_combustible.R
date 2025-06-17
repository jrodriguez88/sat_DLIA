## Biomas CAR
biomas_car <- read_sf("data/raw/car/14_biomas.gpkg")


biomas_car$BIOMA %>% unique()
biomas_car$TIPO_BIOMA %>% unique()


ecosistemas_ideam_2017 <- read_sf("data/raw/car/1_ecosistemas__ideam_año_2017.gpkg")
ecosistemas_ideam_2024 <- read_sf("data/interm/coberturas/clc_car_ecosistemas_ideam_2024.gpkg")
clc_ideam_2020 <- read_sf("data/interm/coberturas/clc_car_2020.gpkg")

ecosistemas_ideam_2017$GRAN_BIOMA%>% unique()


ecosistemas_ideam_2024$gran_bioma %>% unique()
ecosistemas_ideam_2024$bioma_prel %>% unique()
ecosistemas_ideam_2024$bioma_IAvH %>% unique()


fuel_bed_parameters %>% filter(Biome %in% c("1 - Trop/Sub Moist Broadleaf Forest", 
                                            "2 - Trop/Sub Dry Broadleaf Forest",
                                            "11 - Tundra")) %>%
  pull(LandCover) %>% unique()

ecosistemas_ideam_2024$cob %>% unique()



###


biomas_pet <- c(
  "1 - Trop/Sub Moist Broadleaf Forest",
  "2 - Trop/Sub Dry Broadleaf Forest",
  "11 - Tundra"
)

params_region <- fuel_bed_parameters %>%
  filter(Biome %in% biomas_pet) %>%           # Subconjunto por bioma :contentReference[oaicite:0]{index=0}
  distinct(JOIN_VALUE, LandCover, .keep_all = TRUE)


mapeo_cob <- tribble(
  ~cob,                                                    ~LandCover,
  # 1. Cobertura herbácea
  "Herbazal Denso",                                         "140 - Grasses",
  "Herbazal Abierto",                                       "140 - Grasses",
  
  # 2. Cobertura arbustiva
  "Arbustal Denso",                                         "130 - Shrubs",
  "Arbustal Abierto",                                       "130 - Shrubs",
  
  # 3. Pastizales y forrajes
  "Pastos",                                                 "18  - Crops: forage",
  "Mosaico de Pastos y Espacios Naturales",                 "110 - Mosaic trees-shrubs/grasses",
  
  # 4. Mosaicos mixtos
  "Mosaico de Cultivos y Pastos",                           "28  - Mosaic Crops/Vegetation: forage",
  "Mosaico de Cultivos, Pastos y Espacios Naturales",       "110 - Mosaic trees-shrubs/grasses",
  "Mosaico de Cultivos y Espacios Naturales",               "110 - Mosaic trees-shrubs/grasses",
  
  # 5. Cultivos
  "Papa",                                                   "17  - Crops: tubers",
  "Arroz",                                                  "11  - Crops: rice",
  "Café",                                                   "40  - Broadleaf evergreen or semidec. trees",
  "Palma de Aceite",                                        "40  - Broadleaf evergreen or semidec. trees",
  "Cultivos Permanentes",                                   "40  - Broadleaf evergreen or semidec. trees",
  "Cultivos Transitorios",                                  "110 - Mosaic trees-shrubs/grasses",
  "Cultivos Confinados",                                     NA_character_,
  
  # 6. Bosques y vegetación arbórea
  "Bosque Denso Alto",                                      "40  - Broadleaf evergreen or semidec. trees",
  "Bosque Denso Bajo",                                      "40  - Broadleaf evergreen or semidec. trees",
  "Bosque Abierto Alto",                                    "110 - Mosaic trees-shrubs/grasses",
  "Bosque Abierto Bajo",                                    "120 - Mosaic grasses/trees-shrubs",
  "Bosque de Galeria y Ripario",                            "160 - Trees regularly flooded (fresh water)",
  "Bosque Fragmentado con Vegetación Secundaria",           "110 - Mosaic trees-shrubs/grasses",
  "Bosque Fragmentado con Pastos y Cultivos",               "110 - Mosaic trees-shrubs/grasses",
  "Vegetación Secundaria o en Transición",                  "110 - Mosaic trees-shrubs/grasses",
  "Plantación Forestal",                                    "40  - Broadleaf evergreen or semidec. trees",
  
  # 7. Zonas de agua y humedales
  "Ríos (50 m)",                                             NA_character_,
  "Canales",                                                NA_character_,
  "Lagunas, Lagos y Ciénagas",                             NA_character_,
  "Cuerpos de Agua Artificiales",                          NA_character_,
  "Vegetación Acuática sobre Cuerpos de Agua",             NA_character_,
  "Zonas Pantanosas",                                       "180 - Shrubs-grasses regularly flooded",
  
  # 8. Otras coberturas
  "Páramo",                                                 "11  - Tundra",
  "Turberas",                                               NA_character_,
  "Afloramientos Rocosos",                                  NA_character_,
  "Zonas Arenosas Naturales",                               NA_character_,
  "Territorio Artificializado",                             NA_character_
)




# 2. Extrae sólo las columnas que necesitas de fuel_bed_parameters
params_sel <- fuel_bed_parameters %>%
  select(JOIN_VALUE, Biome, LandCover) %>% 
  filter(Biome %in% c("1 - Trop/Sub Moist Broadleaf Forest", 
                      "2 - Trop/Sub Dry Broadleaf Forest",
                      "11 - Tundra")) %>% 
  distinct()

# 3. Haz el join para obtener el JOIN_VALUE real
mapeo_real <- mapeo_cob %>%
  left_join(params_sel, by = "LandCover") %>%
  select(cob, JOIN_VALUE)


# 4. Verifica que todos emparejen
if(any(is.na(mapeo_real$JOIN_VALUE))){
  warning("Faltan algunos JOIN_VALUE: revisa que los LandCover coincidan exactamente.")
}

print(mapeo_real)


ecos_map <- ecosistemas_ideam_2024 %>%
  left_join(mapeo_cob, by = "cob")     # Join por columna ‘cob’ :contentReference[oaicite:1]{index=1}

# Verifica que no queden NA en JOIN_VALUE
stopifnot(!any(is.na(ecos_map$JOIN_VALUE)))



template   <- fuel_bed_car             # Tu raster de Pettinari recortado
vect_ecos  <- vect(ecos_map)           # sf → SpatVector :contentReference[oaicite:2]{index=2}


ecos_map %>% slice(1:30) 

rast_join  <- rasterize(
  vect_ecos,
  template,
  field   = "JOIN_VALUE",
  touches = TRUE                       # Evita huecos en polígonos delgados :contentReference[oaicite:3]{index=3}
)



# Ejemplo para Tree Cover (%)
params_tc <- params_region %>%
  select(JOIN_VALUE, `Tree Cover (%)`)

params_TO_Cover <- params_region %>%
  select(JOIN_VALUE, `TO_Cover (%)`)

rcl_mat <- as.matrix(params_tc)     # 2 columnas: is-becomes :contentReference[oaicite:4]{index=4}
rcl_mat2 <- as.matrix(params_TO_Cover)


rast_tree  <- classify(
  rast_join,
  rcl = rcl_mat                       # Reclasifica códigos a cobertura arbórea :contentReference[oaicite:5]{index=5}
)
to_cover  <- classify(
  rast_join,
  rcl = rcl_mat2                       # Reclasifica códigos a cobertura arbórea :contentReference[oaicite:5]{index=5}
)


# Repite para Shrub Cover, Grass Cover, etc.
plot(rast_tree)

plot(to_cover)


freq(as.factor(rast_join)) %>% arrange(desc(value))

freq((rast_join)) %>% arrange(desc(count))
freq((rast_join)) %>% arrange(desc(value))


plot(rast_join == 2147483648)




# 1. Prepara tu tabla de parámetros sólo con JOIN_VALUE y Tree Cover (%)
params_tc <- fuel_bed_parameters %>%
  select(JOIN_VALUE, `Tree Cover (%)`) %>%
  distinct()

fuel_bed_parameters$JOIN_VALUE %>% max

freq(as.factor(mask(rast_join, rast_join < 10000000)))$value %>% unique 

levels(as.factor(rast_join))[[1]]$Global_fuelbeds_map_Tile3 %>% unique()



freq(as.factor(mask(rast_join, rast_join < 10000000)))$value %>% unique 

# 2. Renombra columnas para usar con subs()
#    “JOIN_VALUE” será la columna de ID que aparece en tu raster base,
#    “TreeCoverPct” el nombre del nuevo campo con los porcentajes.
lookup_tc <- params_tc %>%
  rename(id = JOIN_VALUE,
         TreeCoverPct = `Tree Cover (%)`)

# 3. Ejecuta subs() para intercambiar códigos por porcentajes
# 3. Usa terra::subst() para sustituir códigos por porcentajes
rast_tree_pct <- terra::subst(
  x    = as.factor(rast_join),         # SpatRaster con JOIN_VALUE
  from = as.character(lookup_tc$id),      # valores a buscar
  to   = lookup_tc$TreeCoverPct,
  others = NA               # opcional: asigna NA a valores no coincidentes
)

# 4. Ajusta nombre y revisa
names(rast_tree_pct) <- "TreeCoverPct"
print(rast_tree_pct)


plot(rast_tree_pct)

