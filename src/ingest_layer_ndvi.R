## NDVI from MODIS


dir.create(dir_modis_ndvi)


ndvi_col <- rast(list.files("data/raw/modis/NDVI_col/", full.names = TRUE))


ndvi_car <- recortar_raster_con_shapefile(ndvi_col, jurisdiccion_car)



# ndvi_car %>% plot 


writeRaster(ndvi_car, filename = paste0(dir_modis_ndvi, "/modis_ndvi_car.tif"), overwrite = TRUE)
