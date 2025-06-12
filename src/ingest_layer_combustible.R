library(terra)


library(tidyverse)
library(sf)


jurisdiccion_car <- vect("data/Jurisdiccion_CAR_198535255383287163/Jurisdiccion_CAR.shp")
jurisdiccion_car <- st_as_sf(jurisdiccion_car)
jurisdiccion_car_4326 <- st_transform(jurisdiccion_car, crs = 4326) %>%
  group_by(Direccion) %>%
  summarise(geometry = st_union(geometry))

tile3 <- "data/Global_fuelbeds_map_Tile3/Global_fuelbeds_map_Tile3.tif"

fuel_bed <- rast(tile3)
plot(fuel_bed)
fuel_bed_car <- fuel_bed %>% crop(jurisdiccion_car_4326, mask = T)

plot(fuel_bed_car)
freq(fuel_bed_car)



fuel_bed %>% hist()

fuel_bed %>% summary

fuel_bed_car[fuel_bed_car]

