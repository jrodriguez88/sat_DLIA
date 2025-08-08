## Capas base

base_layers <- c(
  "Límite Departamental",
  "Municipios",
  "Direcciones Regionales",
  "Áreas Naturales Protegidas",
  "Veredas",
  "Drenaje Doble",
  "Drenaje sencillo",
  "Embalse",
  "Vía",
  "Cuencas CAR",
  "Centro poblado DANE"
)



# layers files_in_folder$layer[files_in_folder$layer %in% base_layers]


files_in_folder_sel <- files_in_geocar %>% dplyr::filter(layer %in% base_layers)

layers_mapa <- map(paste0(dir_geocar, files_in_folder_sel$file_name),
                   st_read) %>% setNames(files_in_folder_sel$layer) %>% 
  map(~st_transform(.x, crs = 4326))
# names(layers_mapa)
# layers_mapa %>% map(names)

# Area de jurisdiccion de la CAR
jurisdiccion_car <- layers_mapa[["Direcciones Regionales"]]


## Layers del mapa

#3 Eventos de incendios, ubicacion

# eventos_criticos <- cleaning_puntos_criticos(
#   layers_eventos_dgoat,
#   clase = "Incendio Forestal") %>% st_transform(., crs = 4326)


municipios <- layers_mapa[["Municipios"]]
centro_poblados <- layers_mapa[["Veredas"]] %>% filter(NOMBRE_VER == "CASCO URBANO")
veredas <- layers_mapa[["Veredas"]]
areas_protegidas <- layers_mapa[["Áreas Naturales Protegidas"]]
cuencas <- layers_mapa[["Cuencas CAR"]]
drenaje_doble <- layers_mapa[["Drenaje Doble"]]
drenaje_sencillo <- layers_mapa[["Drenaje sencillo"]]
embalses <- layers_mapa[["Embalse"]]
vias <- layers_mapa[["Vía"]] %>% filter(TIPO_VIA  %in% c(1:4))


# Save layers (municipios, centros_poblados, veredas, etc) in gpkg format in folder : "data/interm/base/"


mapa_test_base <- ggplot() +
  geom_sf(data = areas_protegidas, fill = "darkgreen") +
  geom_sf(data = veredas %>% 
            filter(NOMBRE_VER == "CASCO URBANO"), fill = "brown") +
  geom_sf(data = areas_protegidas, fill = "darkgreen") +
  geom_sf(data = veredas, color = "darkgrey", fill = NA) +
  # geom_sf(data = eventos_criticos, color = "black", size = 1.5, shape = 15) +
  geom_sf(data = municipios, color = "red", linewidth = 1, fill = NA) + 
  geom_sf(data = denaje_doble, color = "blue", linewidth = 1, fill = NA) + 
  # geom_sf(data = vias, color = "gray", linewidth = 1, fill = NA) + 
  #  geom_sf_text(data = st_centroid(veredas), aes(label = NOMBRE_VER), size = 2.5) +
  #  geom_sf_label(areas_protegidas, aes(label = NOMBRE)) +
  #  geom_sf(data = centro_poblado, fill = "brown") +
  theme_minimal() +
  labs(title = "Mapa de prueba - Capas Base")




## Save layers 

layers_to_save <- list(
  municipios         = municipios,
  centros_poblados   = centro_poblados,
  veredas            = veredas,
  areas_protegidas   = areas_protegidas,
  cuencas            = cuencas,
  drenaje_doble      = denaje_doble,
  drenaje_sencillo   = drenaje_sencillo,
  embalses           = embalses,
  vias               = vias,
  # eventos_criticos   = eventos_criticos,
  jurisdiccion_car   = jurisdiccion_car
)


# dir_base <- "data/interm/base"
dir.create(dir_base)

# if(isTRUE(guardar_base)){}

if(length(list.files(dir_base)) == length(layers_to_save)){
  
} else if(length(list.files(dir_base)) == 0){
  # 3. Itera y guarda cada capa como .gpkg con el mismo nombre
  iwalk(layers_to_save, ~ {
  sf::st_write(
    obj      = .x,
    dsn      = file.path(dir_base, paste0(.y, ".gpkg")),
    delete_dsn = TRUE,    # sobrescribe si ya existía
    quiet    = TRUE
  )
})
  
}

















