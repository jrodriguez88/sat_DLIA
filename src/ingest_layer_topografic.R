## Variables de Terreno basadas en DEM - NASADEM

terrain_files <- list.files("data/raw/nasa/NASADEM/", full.names = TRUE)

# ## Jurisdiccion
# jurisdiccion_car <- layers_geocar[["Direcciones Regionales"]] %>%
#   st_transform(., crs = 4326)

names_to_save <- basename(terrain_files) %>% str_to_lower()

rast_terrain <- map(terrain_files, rast) %>% 
  map(~crop(., jurisdiccion_car, mask= T))

# names(rast_terrain)

plot(rast(rast_terrain))
#lines(jurisdiccion_car)
# plot(crop(rast_terrain, jurisdiccion_car, mask= T))


map2(rast_terrain, paste0(dir_terreno, names_to_save), 
     writeRaster, overwrite = TRUE)


# plot(rast("data/interm/terreno/elevacion.tif"))



