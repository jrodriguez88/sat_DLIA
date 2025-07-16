## IDEAM - ECOSISTEMAS 2024 ----


# Lee capa
cob_2024 <- read_sf("data/interm/coberturas/clc_car_ecosistemas_ideam_2024.gpkg")

cob_2024$gran_bioma %>% unique()
cob_2024$bioma_prel %>% unique()
cob_2024$ecos_gener %>% unique()
cob_2024$ecos_sinte %>% unique()
cob_2024$paisaje %>% unique()
cob_2024$clima %>% unique()
cob_2024$cob  %>% unique()


# gran_bioma
# bioma_prel
# ecos_sinte
# ecos_gener
# clima
# paisaje
# cob
# 


# Vector con los nombres de las variables (factores)
vars <- c("gran_bioma", "bioma_prel", "ecos_sinte", "ecos_gener", "clima", "paisaje", "cob")

# Calcular el área total del objeto
total_area <- sum(st_area(cob_2024))

# Función que, dado el nombre de una variable, devuelve un tibble con:
#   - nivel de la variable
#   - área de ese nivel
#   - porcentaje sobre el área total
summarize_area_pct <- function(var) {
  cob_2024 %>%
    # agrupar por el factor dinámico
    group_by(!! sym(var)) %>%
    # unir geometrías y soltar grupos
    summarise(geometry = st_union(geom), .groups = "drop") %>%
    # calcular área y porcentaje
    mutate(
      area = st_area(geometry),
      pct  = as.numeric(area) / as.numeric(total_area) * 100
    ) %>%
    # opcional: eliminar la geometría para ver solo la tabla
    st_drop_geometry() %>%
    # ordenar de mayor a menor porcentaje
    arrange(desc(pct)) %>%
    # renombrar columna del factor a algo genérico "nivel"
    rename(nivel = !! sym(var))
}

# Aplicar sobre todas las vars y guardarlo en una lista nombrada
area_por_var <- map(vars, summarize_area_pct) %>% set_names(vars)

# Ejemplo: ver el summary para "gran_bioma"
area_por_var$gran_bioma








# area_por_var: lista nombrada de tibbles con columnas nivel y pct

# Función que genera el ggplot para un data frame y su nombre
make_bar_plot <- function(df, var_name) {
  ggplot(df, aes(x = fct_reorder(nivel, pct), y = pct)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Distribución de área (%) por nivel de «", var_name, "»"),
      x = var_name,
      y = "Porcentaje de área"
    ) +
    theme_minimal()
}

# Crear una lista de plots, uno por variable
plots_por_var <- imap(area_por_var, ~ make_bar_plot(.x, .y))

# Para mostrarlos en la consola de RStudio o similar:
walk(plots_por_var, print)

# — Opcional: guardar cada plot a disco —
iwalk(plots_por_var, ~ {
  ggsave(
    filename = paste0("pct_area_", .y, ".png"),
    plot     = .x,
    width    = 6,
    height   = 4,
    dpi      = 300
  )
})



### Corine Land Cover crop

library(terra)
library(sf)
library(tmap)

dir.create("data/raw/ideam/coberturas")
dir.create(dir_coberturas)

# 2. Obtener los índices con map_dbl (devuelve numérico) o map_int (entero)
id_layers_mapa <- which(geoCAR_layers$layer == "Direcciones Regionales")

# Descarga mapa geoservicios CAR
layers_mapa <- map(id_layers_mapa, ~load_arcgis_layer(service_url = geoCAR_layers$url_service[.x], 
                                                      layer_id = geoCAR_layers$id[.x],
                                                      layer_name =  geoCAR_layers$layer[.x], 
                                                      epsg = geoCAR_layers$EPSG[.x]
))

jurisdiccion_car <- layers_mapa[[1]]

shape_crop <- jurisdiccion_car

path_list <- list(
  
  path_shp_clc_2000_2002 = "data/raw/ideam/coberturas/Cobertura_tierra_2000_2002/shape/cobertura_tierra2000_2002V2.shp",
  path_shp_clc_2005_2009 = "data/raw/ideam/coberturas/Cobertura_tierra_2005_2009/shape/cobertura_tierra2005_2009.shp",
  path_shp_clc_2010_2012 = "data/raw/ideam/coberturas/Cobertura_tierra_2010_2012/SHP/Cobertura_tierra_2010_2012.shp",
  path_shp_clc_2018 = "data/raw/ideam/coberturas/COBERTURAS CORINE 2018/shape coberturas 2018/cobertura_tierra_clc_2018.shp",
  path_shp_clc_2020 = "data/raw/ideam/coberturas/Cobertura_de_la_tierra_100K_Periodo_2020_limite_administrativo/Shape limite Ambiental/e_cobertura_tierra_2020_amb.shp"
  
)

# poly_shp <- caqueta_shp

## Elegir uno 

# clc_data <- vect(path_shp_clc_2000_2002)
# clc_data <- vect(path_shp_clc_2005_2009)
# clc_data <- vect(path_shp_clc_2010_2012)
# clc_data <- vect(path_shp_clc_2018)
clc_data <- vect(path_shp_clc_2020)



tag <- names(path_list) %>% str_remove_all("path_shp_clc_")


lee_recorta_clc <- function(path, tag) {
  # 1. Leer
  clc_sf <- st_read(path, quiet = TRUE)
  
  # 2. Validar y reparar
  if (any(!st_is_valid(clc_sf))) {
    clc_sf <- st_make_valid(clc_sf)
  }
  
  # 3. Reproyectar
  clc_sf <- st_transform(clc_sf, st_crs(jurisdiccion_car))
  
  # 4. Recortar (topológico)
  clc_recortado_sf <- st_intersection(clc_sf, jurisdiccion_car)
  
  # 5. Guardar
  st_write(clc_recortado_sf,
           paste0("data/interm/coberturas/clc_car_", tag, ".gpkg"),
           delete_layer = FALSE)
  
}

map2(path_list, tag, lee_recorta_clc)





clc_data <- vect(path_shp_clc_2005_2009)

# Revisar geometrías inválidas
invalidas <- which(!is.valid(clc_data))
if (length(invalidas)) {
  cat("Corrigiendo", length(invalidas), "geometrías inválidas...\n")
  clc_data[invalidas, ] <- buffer(clc_data[invalidas, ], width=0)
}

# Verificar si las geometrías están vacías
if (nrow(clc_data) == 0) {
  stop("El shapefile no contiene geometrías válidas después de la corrección.")
}



# Verificar los CRS y reproyectar si es necesario
if (!identical(crs(clc_data), crs(shape_crop))) {
  clc_data <- project(clc_data, crs(shape_crop))
}


# Recortar el shapefile grande usando el shapefile pequeño
clc_recortado <- terra::intersect(clc_data, shape_crop)



# Guardar el shapefile recortado 


writeVector(clc_recortado, "data/interm/coberturas/clc_car_2005_2009_2.gpkg", overwrite=TRUE)

clc_recortado



# Cargar el shapefile usando sf
cobertura <- st_read("data/interm/coberturas/clc_car_2000_2002.gpkg")

# Crear un mapa temático usando tmap
tmap_mode("plot")

# Asignar colores a las diferentes categorías de cobertura de suelo
mapa_cobertura <- tm_shape(cobertura) +
  tm_polygons("LEYENDA", 
              title = "Cobertura de Tierra", 
              palette = "Set3", 
              style = "cat") +  # "cat" indica que es una variable categórica
  tm_layout(legend.outside = TRUE)

# Mostrar el mapa
print(mapa_cobertura)






