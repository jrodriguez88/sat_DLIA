## data requirements ----

# base_modelo <- df
# 
# raster_coberturas <- raster_clc_car[[5]]
# 
# 
# 




## Preprocess - occurrence data - from DGOAT

prepare_data_dgoat <- function(data_ocurrencia){
  
  
df <- data_ocurrencia %>%
  mutate(
    Fecha_Inicio = as.Date(as.numeric(Fecha_Inicio), origin = "1899-12-30"),
    Fecha_Final  = as.Date(as.numeric(Fecha_Final),  origin = "1899-12-30"),
    Hora_Inicio  = hms::as_hms(as.numeric(Hora_Inicio) * 86400),
    Hora_Final   = hms::as_hms(as.numeric(Hora_Final)  * 86400),
    Area_Afectada = as.numeric(Area_Afectada)
  ) %>% filter(Area_Afectada > 0)


eventos_ocurrencia <- df %>% 
  dplyr::select(OBJECTID:Fecha_Inicio, Area_Afectada) %>% 
  drop_na() %>%
  dplyr::group_by(Este, Norte, Fecha_Inicio, Area_Afectada) %>%
  dplyr::slice_head(n = 1) %>%        # conserva el primer OBJECTID de cada grupo
  dplyr::ungroup() %>% arrange(OBJECTID) %>% rename(Fecha = Fecha_Inicio)

# eventos_intensidad <- df %>%
#   tidyr::drop_na(Este:Area_Afectada) %>%
#   dplyr::group_by(Este, Norte, Fecha_Inicio, Area_Afectada) %>%
#   dplyr::slice_head(n = 1) %>%        # conserva el primer OBJECTID de cada grupo
#   dplyr::ungroup()


# write_csv(eventos_ocurrencia, "data/final/ocurrencia_eventos_revised.csv")



# 4. Asignar EPSG:3116 y transformar los datos de incendios a EPSG:9377
sf_obj_3116 <- st_as_sf(eventos_ocurrencia, coords = c("Este", "Norte"), crs = 3116)
eventos_ocurrencia_espacial <- st_transform(sf_obj_3116, crs = 9377)

# eventos_ocurrencia_espacial %>% slice(which.max(eventos_ocurrencia_espacial$Area_Afectada))
# 
# eventos_ocurrencia_espacial$Area_Afectada %>% hist
# log2(eventos_ocurrencia_espacial$Area_Afectada) %>% hist
# 
# eventos_ocurrencia_espacial$Area_Afectada[eventos_ocurrencia_espacial$Area_Afectada < 50] %>% 
#   hist(main = "Eventos < 50 ha")
# 
# eventos_ocurrencia_espacial$Area_Afectada[eventos_ocurrencia_espacial$Area_Afectada < 5] %>% 
#   hist(main = "Eventos < 5 ha")


# plot(st_geometry(eventos_ocurrencia_espacial))


# Se observan datos fuera de jurisdiccion (erro en coordenadas)

# eventos_ocurrencia_espacial

limites <- jurisdiccion_car %>% st_transform(crs = 9377)

eventos_car <- sf::st_filter(eventos_ocurrencia_espacial, limites, .predicate = sf::st_within) %>%
  dplyr::filter(Fecha < dmy("01/01/2024"))


eventos_car

}




extrae_data_training_model <- function(base_modelo, 
                                       raster_distancias_car, 
                                       rast_terrain, raster_coberturas, 
                                       ndvi_car, 
                                       precipitacion, temp_max, 
                                       temp_min, humedad_rel, 
                                       socioeconomic_data_spatial, 
                                       punto_o_vecino = "_valor"){

  

## Extraccion de datos basado en distancias y terreno ----

layers_distancias <- names(raster_distancias_car)

extract_distacias_incendios_car <- map(layers_distancias, ~extrae_estadisticas_espaciales(
  geom_extract = base_modelo,
  base = raster_distancias_car[[.x]],        # tu SpatRaster 'base_raster'
  id_col = NULL,
  radio_celdas = 1            # Moore 3x3
))

layer_terrain <- names(rast(rast_terrain))
layer_terrain <- c("elevacion", "orientacion", "pendiente", "sombreado")

extract_terreno_incendios_car <- map(names(rast(rast_terrain)), ~extrae_estadisticas_espaciales(
  geom_extract = base_modelo,
  base = rast(rast_terrain)[[.x]],        # tu SpatRaster 'base_raster'
  id_col = NULL,
  radio_celdas = 1            # Moore 3x3
))

extract_cobertura_incendios_car <- extrae_estadisticas_espaciales(
  geom_extract = base_modelo,
  base = raster_coberturas,        # tu SpatRaster 'base_raster'
  id_col = NULL,
  radio_celdas = 1            # Moore 3x3
)

distacias_incendios_car <- extract_distacias_incendios_car %>% 
  setNames(layers_distancias) %>%
  bind_rows(.id = "predictor") %>%
  pivot_wider(
    names_from = predictor,
    values_from = c(valor, vecindad),
    names_glue = "{predictor}_{.value}"
  )

terreno_incendios_car <- extract_terreno_incendios_car %>% 
  setNames(layer_terrain) %>%
  bind_rows(.id = "predictor") %>%
  pivot_wider(
    names_from = predictor,
    values_from = c(valor, vecindad),
    names_glue = "{predictor}_{.value}"
  )

# plot(raster_clc_car[[5]], type = "classes", col=terrain.colors(20))

coberturas_incendios_car <- extract_cobertura_incendios_car %>% 
  mutate(predictor = "cobertura") %>%
  pivot_wider(
    names_from = predictor,
    values_from = c(valor, vecindad),
    names_glue = "{predictor}_{.value}"
  )



# fuel_bed_raster <- rast(raster_combust)
# layer_names_combustible <- names(fuel_bed_raster)
# 
# 
# extract_combustible_incendios_car <- map(layer_names_combustible, ~extrae_estadisticas_espaciales(
#   geom_extract = base_modelo,
#   base = fuel_bed_raster[[.x]],        # tu SpatRaster 'base_raster'
#   id_col = NULL,
#   radio_celdas = 1            # Moore 3x3
# ))
# 
# # combustible_incendios_car <- extract_combustible_incendios_car[-1] %>% 
# #   setNames(layer_names_combustible[-1]) %>% #enframe() %>% pivot_wider(id_cols = -name)
# #   bind_rows(.id = "predictor") %>%
# #   # mutate(predictor = "combustible") %>%
# #   pivot_wider(
# #     names_from = predictor,
# #     values_from = c(valor, vecindad),
# #     names_glue = "{predictor}_{.value}"
# #   )
# 
# # combustible_incendios_car
# 
# combustible_incendios_car <- imap(extract_combustible_incendios_car[-1] %>% 
#                                     setNames(layer_names_combustible[-1]) , function(df, name) {
#                                       cols_to_rename <- intersect(c("valor", "vecindad"), names(df))
#                                       df %>% rename_with(~ paste0(name, "_", .x), .cols = cols_to_rename)
#                                     }) %>% reduce(left_join)
# 
# to_pca_combustible <- dplyr::select(combustible_incendios_car, where(is.numeric)) %>% 
#   dplyr::select(contains("valor") , -contains("type")) %>% 
#   dplyr::select(where(~ var(., na.rm = TRUE) != 0)) %>% drop_na()



## Extract data based - NDVI ----

# ndvi_car   ---aca necesita ajuste si la entrada es variable 
ndvi_names <- as.vector(sapply(2001:2023, function(y) paste0("NDVI_", month.abb, "_", y)))
set.names(ndvi_car, ndvi_names)

# ndvi_car[[names(ndvi_car) %>% str_detect("2019")]]

# 1) Asegura CRS compatible (ndvi_car está en WGS84 lon/lat)
eventos_wgs <- st_transform(base_modelo, crs(ndvi_car))

# 2) Nombres de capa para el mes del evento y el mes previo
mes_evt  <- month(eventos_wgs$Fecha, label = TRUE, abbr = TRUE, locale = "C") |> as.character()
anio_evt <- year(eventos_wgs$Fecha)

fecha_prev <- eventos_wgs$Fecha %m-% months(1)
mes_prev   <- month(fecha_prev, label = TRUE, abbr = TRUE, locale = "C") |> as.character()
anio_prev  <- year(fecha_prev)

name_cur  <- paste0("NDVI_", mes_evt,  "_", anio_evt)
name_prev <- paste0("NDVI_", mes_prev, "_", anio_prev)

# 3) Extrae solo las capas necesarias (únicas) y toma valores para cada punto
capas_nec <- unique(c(name_cur, name_prev))
ndvi_sub  <- ndvi_car[[capas_nec]]

valores <- terra::extract(ndvi_sub, terra::vect(eventos_wgs))/10000  # data.frame con columnas = capas
valores <- valores[,-1, drop = FALSE]                          # quita columna ID

# 4) Construye salida por fila (valor del mes del evento y del mes previo)
out_NDVI <- eventos_wgs %>%
  st_drop_geometry() %>%
  mutate(
    capa_cur  = name_cur,
    capa_prev = name_prev,
    NDVI_evt  = valores[cbind(seq_len(n()), match(capa_cur,  colnames(valores)))],
    NDVI_prev = valores[cbind(seq_len(n()), match(capa_prev, colnames(valores)))]
  ) %>%
  select(contains("id"), Fecha,  NDVI_evt, NDVI_prev)



## Extract Climate data from IDEAM ----

# 0) Asegura CRS compatible con los rasters (WGS84)
eventos_wgs <- st_transform(base_modelo, crs(precipitacion))

# 1) Índices de capa por mes/año
t_prcp <- as.Date(time(precipitacion))
t_tmax <- as.Date(time(temp_max))
t_tmin <- as.Date(time(temp_min))
t_humd <- as.Date(time(humedad_rel))

m_cur  <- floor_date(eventos_wgs$Fecha, "month")
m_prev <- m_cur %m-% months(1)

idx <- function(d, tt) match(paste(year(d), month(d)), paste(year(tt), month(tt)))
i_prcp_cur  <- idx(m_cur,  t_prcp);  i_prcp_prev  <- idx(m_prev, t_prcp)
i_tmax_cur  <- idx(m_cur,  t_tmax);  i_tmax_prev  <- idx(m_prev, t_tmax)
i_tmin_cur  <- idx(m_cur,  t_tmin);  i_tmin_prev  <- idx(m_prev, t_tmin)
i_humd_cur  <- idx(m_cur,  t_humd);  i_humd_prev  <- idx(m_prev, t_humd)

# 2) Extrae valores para todos los meses (rápido: grilla pequeña)
P  <- terra::extract(precipitacion, terra::vect(eventos_wgs))
TX <- terra::extract(temp_max,      terra::vect(eventos_wgs))
TN <- terra::extract(temp_min,      terra::vect(eventos_wgs))
HR <- terra::extract(humedad_rel,   terra::vect(eventos_wgs))

# 3) Arma salida por fila (ajuste +1 por columna ID que añade extract)
n <- nrow(eventos_wgs)
out_CLIMA <- eventos_wgs %>%
  st_drop_geometry() %>%
  mutate(
    prcp_evt  = P [cbind(seq_len(n), i_prcp_cur  + 1)],
    prcp_prev = P [cbind(seq_len(n), i_prcp_prev + 1)],
    tmax_evt  = TX[cbind(seq_len(n), i_tmax_cur  + 1)],
    tmax_prev = TX[cbind(seq_len(n), i_tmax_prev + 1)],
    tmin_evt  = TN[cbind(seq_len(n), i_tmin_cur  + 1)],
    tmin_prev = TN[cbind(seq_len(n), i_tmin_prev + 1)],
    rh_evt    = HR[cbind(seq_len(n), i_humd_cur  + 1)],
    rh_prev   = HR[cbind(seq_len(n), i_humd_prev + 1)]
  ) %>% dplyr::select(contains("id"), Fecha, prcp_evt:rh_prev)

# out: tibble con variables del mes del evento y del mes previo por punto


## Extract Socioeconomic data ----

# 2) Puntos + Polígono (vector) con atributo numérico
poblacion <- extrae_estadisticas_espaciales(
  geom_extract = base_modelo,
  base = socioeconomic_data_spatial,
  campo_base = "Población (hab) 2025",
  id_col = NULL
) %>% 
  mutate(predictor = "poblacion") %>%
  pivot_wider(
    names_from = predictor,
    values_from = c(valor, vecindad),
    names_glue = "{predictor}_{.value}",
    values_fn = list(valor = mean, vecindad = mean)
  )


# head(poblacion)

educacion <- extrae_estadisticas_espaciales(
  geom_extract = base_modelo,
  base = socioeconomic_data_spatial,
  campo_base = "Educación (% Cobertura)",
  id_col = NULL
)%>%
  # garantiza tipo numérico
  dplyr::mutate(
    valor    = as.numeric(valor),
    vecindad = as.numeric(vecindad)
  ) %>%
  # un registro por .oid (promedio si hay duplicados)
  dplyr::group_by(.oid) %>%
  dplyr::summarise(
    educacion_valor    = mean(valor,    na.rm = TRUE),
    educacion_vecindad = mean(vecindad, na.rm = TRUE),
    .groups = "drop"
  )
# head(educacion)

pobreza <- extrae_estadisticas_espaciales(
  geom_extract = base_modelo,
  base = socioeconomic_data_spatial,
  campo_base = "Pobreza (%) 2023",
  id_col = NULL
) %>% 
  mutate(predictor = "pobreza") %>%
  pivot_wider(
    names_from = predictor,
    values_from = c(valor, vecindad),
    names_glue = "{predictor}_{.value}",
    values_fn = list(valor = mean, vecindad = mean)
  )
# head(pobreza)

municipio <- extrae_estadisticas_espaciales(
  geom_extract = base_modelo,
  base = socioeconomic_data_spatial,
  campo_base = "Municipio",
  id_col = NULL
) %>% mutate(valor = tolower(valor)) %>%
  dplyr::select(.oid, municipio_valor = valor) %>% 
  distinct()
  # un registro por .oid (promedio si hay duplicados)
  # dplyr::group_by(.oid) %>%
  # dplyr::summarise(
  #   municipio_valor  = slice(municipio_valor, 1),
  #   .groups = "drop"
  # )
# head(municipio)

direccion_car <- extrae_estadisticas_espaciales(
  geom_extract = base_modelo,
  base = socioeconomic_data_spatial,
  campo_base = "Direccion",
  id_col = NULL
) %>% mutate(valor = tolower(valor)) %>%
  dplyr::select(.oid, direccion_valor = valor) %>% 
  distinct()
# head(pobreza)


## Join al extrated data ----


data_base_test <- base_modelo %>% 
  mutate(
    longitud = st_coordinates(.)[,1],
    latitud  = st_coordinates(.)[,2]
  ) %>% st_drop_geometry() %>%
  dplyr::select(contains("id"), latitud, longitud, Fecha) %>%
  mutate(#Municipio = toupper(Municipio), 
         .oid = 1:nrow(.)) %>%
  left_join(
    left_join(distacias_incendios_car, terreno_incendios_car) %>%
      left_join(coberturas_incendios_car) %>% left_join(poblacion) %>%
      left_join(educacion) %>% left_join(pobreza) %>% 
      left_join(municipio) %>% left_join(direccion_car) %>%
      dplyr::select(.oid, contains(punto_o_vecino)) 
    
  ) %>% left_join(out_NDVI) %>% left_join(out_CLIMA) %>%
  tibble() 



#data_base_test[1,18] <- 24

new_names <- data_base_test %>% names() %>% str_remove_all(punto_o_vecino) %>% 
  tolower()


test_data <- data_base_test %>% setNames(new_names) %>% 
  column_to_rownames(".oid") 

tibble(test_data)

}
