
### Ingest DGOAT - Puntos criticos de Geoportal CAR

# Requeriments
# source("src/ingest_layer_geocar_all.R")
# dir_eventos <- "data/interm/eventos_ocurrencia/"
dir.create(dir_eventos)

# dir_satelital <- "data/interm/satelital/"
# dir.create(dir_satelital)

## Puntos criticos
layer_incendios <- "Riesgos - Registros de eventos"
id_layer_incendios <- which(geoCAR_layers$layer == layer_incendios)
#
# # Descarga mapa geoservicios CAR
layers_eventos_dgoat <- load_arcgis_layer(service_url = geoCAR_layers$url_service[id_layer_incendios],
                                          layer_id = geoCAR_layers$id[id_layer_incendios],
                                          layer_name =  geoCAR_layers$layer[id_layer_incendios],
                                          epsg = geoCAR_layers$EPSG[id_layer_incendios])

#


#3 Eventos de incendios, ubicacion 

#3 Eventos de incendios, ubicacion

eventos_dgoat_geoambiental <- cleaning_puntos_criticos(
  layers_eventos_dgoat,
  clase = "Incendio Forestal") %>% 
  st_transform(., crs = 4326)

eventos_dgoat_geoambiental %>%
  st_write(paste0(dir_eventos, "eventos_dgoat_geoambiental.gpkg"), append=FALSE)


# DGOAT _ Interno 
raw_data <- read_excel("data/raw/car/dgoat/REGISTROS_IF.xlsx", sheet = 1)


# Preprocesamiento - Test Data ----
eventos_dgoat_interno <- raw_data %>% 
  select(Municipio, `Vereda Localidad`, Fecha_reporte = `Fecha Reporte`,  Año, Este, Norte, Altura,`Nombre Punto`,  Clase,  `Causas Tipos`, Categoria, Subcategoria,`Area Afectada Ha`) %>%
  mutate(class_area = 
           case_when(
             str_detect(`Area Afectada Ha`, pattern = "m2|metros cuadrados") ~ "m2",
             TRUE ~ "ha")) %>%
  mutate(Fecha_reporte = as.Date(`Fecha_reporte`)) %>%
  mutate(area1 = str_remove_all(`Area Afectada Ha`, "[aA-zZ]*")) %>%
  mutate(area1 = str_replace(area1, ",", ".")) %>%
  mutate(area1 = str_replace(area1, "[ ]+", ""),
         area2 = parse_number(area1)) %>%
  mutate(class_area = ifelse(area2>500, "m2", class_area)) %>%
  mutate(area_metros = case_when(class_area == "m2" ~ area2,
                                 class_area == "ha" ~ area2*10000,
                                 TRUE ~ NA_real_),
         area_hectareas = area_metros/10000) %>%
  mutate(across(.cols = c(Municipio, 
                          Clase,
                          Categoria, Subcategoria,
                          `Causas Tipos`), str_to_title)) %>%
  select(-c(`Area Afectada Ha`, class_area, area1, area2)) %>% 
  distinct()



eventos_dgoat_interno %>%
  write_csv(paste0(dir_eventos, "eventos_dgoat_interno.csv"))



## Ingest UNGRD Data

## Portal: https://www.datos.gov.co/Ambiente-y-Desarrollo-Sostenible/Emergencias-UNGRD-/wwkg-r6te/about_data
## Fuente : http://portal.gestiondelriesgo.gov.co/Paginas/Consolidado-Atencion-de-Emergencias.aspx



## Lectura
data_ungrd_co <- read_csv("https://www.datos.gov.co/resource/wwkg-r6te.csv?$limit=100000")

# Filter for Cundinamarca and Boyaca
data_cundinamarca_boyaca_bogota <- data_ungrd_co %>% 
  filter(evento == "INCENDIO DE COBERTURA VEGETAL",
         departamento %in% c("CUNDINAMARCA", "BOYACA", "BOGOTA, D.C.")) 


## Codigo DIVIPOLA Colombia
municipios_filter <- municipios$CODDANE %>% unique()


eventos_ungrd_datos_abiertos <- data_cundinamarca_boyaca_bogota %>% 
  filter(divipola %in% municipios_filter)


eventos_ungrd_datos_abiertos %>%
  write_csv(paste0(dir_eventos, "eventos_ungrd_datosabiertos.csv"))







### data UNGDR from WP

## Agregar script de importacion y preprocesado de UNGRD

data_ungdr_prep <- read_csv("data/raw/ungrd/UNGRD_data_preprocess_incendios.csv")


data_cundinamarca_boyaca_bogota <- data_ungdr_prep %>%
  filter(departamento %in% str_to_title(c("CUNDINAMARCA", "BOYACA", "BOGOTA, D.C.")))


## Convertir caracteres latinos a general



# data_cundinamarca_boyaca_bogota$municipio %>% unique

municipios_filter <- municipios$Municipio %>% unique() %>%
  stri_trans_general("Latin-ASCII")


eventos_ungrd_gestiondelriesgo <- data_cundinamarca_boyaca_bogota %>% 
  mutate(municipio = str_to_upper(municipio)) %>%
  filter(municipio %in% municipios_filter)



eventos_ungrd_gestiondelriesgo %>%
  write_csv(paste0(dir_eventos, "eventos_ungrd_gestiondelriesgo.csv"))




### Modis  

list_files_layers <- list.files("data/raw/modis/MCD64A1/", pattern = ".tif$", full.names = TRUE) 

names_layers <- str_remove_all(basename(list_files_layers), "MODIS_MCD64A1_BurnDate_") %>%
  str_remove(".tif") %>%
  parse_date()

#compilado todas las imágenes en un solo archivo
modis_mcd64a1 <- rast(list_files_layers) %>% setNames(names_layers) 
# crs(modis_mcd64a1)
raster_crs <- crs(modis_mcd64a1, proj = TRUE)

# compilado todas las imágenes en un solo archivo
modis_mcd64a1 <- rast(list_files_layers) %>% setNames(names_layers) 
# crs(modis_mcd64a1)
raster_crs <- crs(modis_mcd64a1, proj = TRUE)


# jurisdiccion_car <- vect("data/interm/base/jurisdiccion_car.gpkg")

jurisdiccion_car_proj <- st_transform(st_as_sf(jurisdiccion_car), raster_crs) %>%
  group_by(Direccion) %>%
  summarise(geometry = st_union(geom))
# ext(jurisdiccion_car_proj)
# burn_rasters <- modis_mcd64a1 #%>% project(crs(sf_obj_3116)) %>% crop(sf_obj_3116, mask = T)

# app(burn_rasters[1][burn_rasters[1]>0], mean, na.rm = T) %>% plot
# fire_events_car <- st_transform(sf_obj_3116, crs = raster_crs) %>%
#   select(Municipio, Fecha_reporte, area_hectareas, geometry)

available_dates <- names(modis_mcd64a1)


# Cortar Raster al poligono Jurisdiccion CAR
burn_rasters_car <- crop(modis_mcd64a1, vect(jurisdiccion_car_proj), mask = TRUE)


fechas <- as.Date(names(burn_rasters_car), format="%Y-%m-%d")
names(burn_rasters_car) <- fechas


writeRaster(burn_rasters_car, filename = paste0(dir_eventos, "modis_areas_quemadas.tif"), overwrite=TRUE)

#burn_rasters_car <- project(burn_rasters_car, crs(eventos_dgoat) , method = "near")

# Extraer valores con coordenadas
df_raw <- as.data.frame(burn_rasters_car, xy=TRUE) %>% 
  pivot_longer(
    cols = -c(x, y),
    names_to = "fecha",
    values_to = "valor"
  ) %>%
  filter(valor > 0) %>%
  mutate(area = 25) 



sf_raw <- df_raw %>% 
  # st_as_sf convierte a simple feature, usando columnas x e y
  st_as_sf(coords = c("x", "y"), crs = raster_crs)  

# 2. Join espacial -----------------------------------------------------------
# municipios_car <- st_read("data/interm/base/municipios.gpkg")

municipios_to_join <- municipios %>% select(Municipio, CODDANE) %>%
  st_transform(raster_crs)

# st_join con st_intersects (por defecto) hará un left join:
sf_joined <- sf_raw %>% 
  st_join(municipios_to_join, left = TRUE,
          join = st_within)

modis_points <- sf_joined %>% drop_na %>%
  group_by(fecha, Municipio, CODDANE) %>%
  summarise(n_pixeles = n(),
            area_afectada = sum(area),
            dia_juliano = mean(valor),
            geometry = st_union(geometry),
            geometry = st_centroid(geometry), .groups="drop") %>%
  ungroup()



modis_points %>%
  st_write(paste0(dir_eventos, "eventos_modis_mcd64a1.gpkg"), append = FALSE)







### analisis graficos comparacion
# 
# layers_eventos_dgoat %>% tibble() %>% filter(Clase=="Incendio Forestal") %>% 
#   group_by(`Año`) %>% summarise(n = n())

left_join(
  eventos_dgoat_interno%>% tibble() %>% 
    mutate(Municipio = stri_trans_general(Municipio, "Latin-ASCII") %>% toupper()) %>%
    filter(Clase == "Incendio Forestal") %>% 
  group_by(Municipio, year = as.numeric(`Año`)) %>% 
  summarise(n_dgoat = n(),
            area_dgoat = sum(area_hectareas, na.rm = TRUE)) %>%
  arrange(year), 
  
  eventos_ungrd_gestiondelriesgo %>% tibble() %>% 
    mutate(Municipio = stri_trans_general(municipio, "Latin-ASCII") %>% toupper()) %>%
    group_by(Municipio, year) %>% 
  summarise(n_ungdr = n(), 
            area_ungdr = sum(area_hectareas, na.rm = TRUE))
) %>% 
  left_join(
    modis_points %>%
  tibble() %>% 
    mutate(Municipio = stri_trans_general(Municipio, "Latin-ASCII") %>% toupper()) %>%
    group_by(Municipio, year = year(fecha)) %>% 
  summarise(n_modis = n(), 
            area_modis = sum(area_afectada , na.rm = TRUE))
) -> test_data_events


view(test_data_events)


# test_data_events %>% drop_na(area_modis) %>% arrange(desc(year)) %>% view()


plot(test_data_events)




# test_data_events %>% ungroup() %>%
# #  drop_na(area_dgoat, area_ungdr, area_modis) %>%
#   select(area_dgoat, area_ungdr, area_modis) %>%
#   pivot_longer(cols = everything(),
#                names_to  = "metodo",
#                values_to = "area") %>%
#   ggplot(aes(x = metodo, y = area, fill = metodo)) +
#   geom_boxplot() +
#   labs(
#     title = "Comparación de áreas por método",
#     x = "Método",
#     y = "Área (m²)"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# 

# df_long <- test_data_events %>%
#   # drop_na(area_dgoat, area_ungdr, area_modis) %>%
#   # Asegurarnos de que fecha es Date
#   mutate(fecha = make_date(year = year)) %>%
#   # Convertir a formato largo
#   pivot_longer(
#     cols = c(area_dgoat, area_ungdr, area_modis),
#     names_to  = "metodo",
#     values_to = "area"
#   ) %>%
#   # Agrupar por fecha y método y calcular el total de área
#   group_by(fecha, metodo) %>%
#   summarise(
#     total_area = sum(area, na.rm = TRUE),
#     .groups    = "drop"
#   )
# 
# 
# # 2. Graficar con ggplot2: líneas comparativas de total_area por método
# ggplot(df_long, aes(x = fecha, y = total_area, color = metodo)) +
#   geom_line(size = 1) +
#   geom_point(size = 1.5, alpha = 0.7) +
#   scale_y_continuous(labels = label_comma()) +
#   labs(
#     title    = "Comparación de área quemada por método",
#     subtitle = "Suma de área por fecha: DGOAT, UNGDR y MODIS",
#     x        = "Fecha",
#     y        = "Área total (ha)",
#     color    = "Método"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x      = element_text(angle = 45, hjust = 1),
#     panel.grid.minor = element_blank()
#   )


df_events_year <- test_data_events %>%
  drop_na(n_dgoat, n_ungdr, n_modis) %>%
  pivot_longer(
    cols       = c(n_dgoat, n_ungdr, n_modis),
    names_to   = "metodo",
    values_to  = "n_events"
  ) %>%
  group_by(year, metodo) %>%
  summarise(
    total_events = sum(n_events, na.rm = TRUE),
    .groups      = "drop"
  )

df_events_year2 <- test_data_events %>%
#  drop_na(area_dgoat, area_ungdr, area_modis) %>%
  pivot_longer(
    cols       = c(area_dgoat, area_ungdr, area_modis),
    names_to   = "metodo",
    values_to  = "area"
  ) %>%
  group_by(year, metodo) %>%
  summarise(
    total_events = sum(area, na.rm = TRUE),
    .groups      = "drop"
  )

# 2. Gráfico de barras agrupadas
grafico_numero_eventos <- ggplot(df_events_year, aes(x = factor(year), y = total_events, fill = metodo)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Número de eventos por fuente y año",
    x     = "Año",
    y     = "Total de eventos",
    fill  = "Fuente"
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


# 2. Gráfico de barras agrupadas
grafico_area_eventos <- ggplot(df_events_year2, aes(x = factor(year), y = total_events, fill = metodo)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Area afectada por fuente y año",
    x     = "Año",
    y     = "Total Area (ha)",
    fill  = "Fuente"
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )





