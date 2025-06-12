
## TEST POMCAs mosaic
geoCAR

POMCAS_geoCAR <- geoCAR_layers %>% 
  filter(name == "VISOR/POMCAS") %>% 
  mutate(vect_layer = pmap(list(service_url = url_service, 
                                layer_i = id, 
                                layer_name =  layer,
                                epsg  = EPSG), load_arcgis_layer))


# car_layer <- test
# layer_name <- CAR_layers$layer[105]
# out_path <- "data/"
# Convierte vector layer to raster
list_POMCAS <- pmap(list(car_layer = POMCAS_geoCAR$vect_layer,
          layer_name = POMCAS_geoCAR$layer,
          field_to_rast = "DESCRIP", 
          resol = 100,
          out_path = out_path), rasterize_car)



map2(.x = list_POMCAS, .y = POMCAS_geoCAR$layer,  ~plot(.x,  main = .y))

# rsrc = terra::sprc(list_POMCAS)

m <- reduce(list_POMCAS, merge)
plot(m)




