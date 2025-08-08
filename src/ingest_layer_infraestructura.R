## Ingest Layer infraestructura



dir.create(dir_infraestructura)

# https://geo.upme.gov.co/server/rest/services/Capas_EnergiaElectrica
# https://geo.upme.gov.co/server/rest/services/Capas_EnergiaElectrica/Sistema_transmision_lineas_construidas/FeatureServer/17


service_upme <- "https://geo.upme.gov.co/server/rest/services/Capas_EnergiaElectrica/Sistema_transmision_lineas_construidas/FeatureServer/"
id <- 17
name <- "Sistema de transmisión: Líneas construidas"
epsg <- 9377


lineas_electricas <- load_arcgis_layer(service_upme, id, name, epsg)

# plot(lineas_electricas %>% dplyr::select(sistema))





# 2. Validar y reparar
if (any(!st_is_valid(lineas_electricas))) {
  lineas_electricas <- st_make_valid(lineas_electricas)
}

# 3. Reproyectar
lineas_sf <- st_transform(lineas_electricas, st_crs(jurisdiccion_car))

# 4. Recortar (topológico)
lineas_electricas <- st_intersection(lineas_sf, jurisdiccion_car)

# 5. Guardar
st_write(lineas_electricas,
         paste0(dir_infraestructura, "lineas_electricas_car_", "upme", ".gpkg"),
         append = FALSE)

