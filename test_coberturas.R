# library(terra)
# library(dplyr)
library(sf)
library(stringr)


# Argumentos funcion 
clc_level = 2
resol = 250

out_path_coberturas <- "data/interm/coberturas/"


list_shp_clc_car <- list.files(out_path_coberturas, full.names = T, pattern = ".gpkg$")


# Cargar el shapefile choco
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2000_2002.shp")
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2005_2009.shp")
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2010_2012.shp")
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2018.shp")
# clc_shp <- vect("data/ideam/coberturas/choco/clc_choco_2020.shp")


raster_clc_car <- map(.x = list_shp_clc_car[-6], ~rasterize_clc(.x, out_path = out_path_coberturas, clc_level = clc_level))
tags_car <- basename(list_shp_clc_car) %>% str_remove_all("clc_|.shp") 



## Graficos ----

par(mfrow = c(1, 5))
#map(list_shp_clc_caqueta, ~rasterize_clc(.x, out_path = out_path))
map2(raster_clc_car[-6], tags_car[-6], ~plot_raster_clc(.x, clc_level = clc_level, tag = .y))
par(mfrow = c(1, 1))





# writeRaster(raster_cobertura, filename = paste0(out_path, tag,"_", nivel_clc, ".tif"),  overwrite = TRUE)


#plot(rast("data/ideam/coberturas/choco/coberturas_CLC_nivel_3.tif"))

# par(mfrow = c(1, 5))
# 
# plot_raster_clc(raster_clc, clc_level = clc_level)
# plot_raster_clc(raster_clc, clc_level = clc_level)
# plot_raster_clc(raster_clc, clc_level = clc_level)
# plot_raster_clc(raster_clc, clc_level = clc_level)

"#ffe64d"
"#00a600"


## Sett plots
colors_clc <- crear_paleta_clc("nivel_2") %>% mutate(value = codigo_simple) %>%
  dplyr::select(- nivel, - codigo_simple)
names(colors_clc$color) <- colors_clc$leyenda

to_bar_plot_car <- raster_clc_car %>% 
  map(calcula_area_clc) %>%
  set_names(tags_car[-6] %>% str_remove_all("car_")) %>% 
  bind_rows(.id = "Periodo") %>% left_join(colors_clc)


# Departamental

ggplot(to_bar_plot_car , aes(x = Periodo, y = area_total, fill = leyenda)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_clc$color) +
  theme_minimal() +
  labs(title = "Coberturas CLC-CAR",
       x = "Periodo",
       y = "Hectareas", 
       fill = "Categoria")



