
# # Cargar librerías necesarias
# # library(leaflet)
# # library(sf)
# # library(terra)
# # library(RColorBrewer)
# # library(viridis)
# # library(tidyverse)
# 
# # 1. Un vector con todos los nombres de layer
# geoCAR_layers$layer
# 
layer_seleccionadas <- c(
  "Límite Departamental",
  "Municipios",
  "Direcciones Regionales",
  "Áreas Naturales Protegidas",
  "Ecosistemas - IDEAM año 2017",
  "Veredas",
  "Drenaje Doble",
  "Drenaje sencillo",
  "Embalse",
  "Vía",
  "Cuencas CAR",
  "Centro poblado DANE"

)


# 
# 
# #1.1. Selecciona el municipio de interes
# sel_municipios <- c("RICAURTE", "NILO", "GUADUAS", "BOJACÁ", "RÁQUIRA")
# sel_municipios <- c("GUADUAS")
# 
# ## cabeceras
# ## cuerpos hidricos
# ## 
# 
# # 2. Obtener los índices con map_dbl (devuelve numérico) o map_int (entero)
# id_layers_mapa <- map_int(layer_names, ~ which(geoCAR_layers$layer == .x))



# # reombra y reprojecta
# # layers_mapa <- setNames(layers_mapa, layer_names) %>% map(~st_transform(.x, crs = 4326))
# 
# layers_mapa <- layers_geocar[layer_seleccionadas] %>% 
#   map(~st_transform(.x, crs = 4326))
# names(layers_mapa)
# layers_mapa %>% map(names)

# Area de jurisdiccion de la CAR
# jurisdiccion_car <- layers_mapa[["Direcciones Regionales"]]

# DEM <- geodata::elevation_30s(country = "COL", path=tempdir())
# municipio_dem <- crop(project(DEM, crs(jurisdiccion_car)), jurisdiccion_car, mask = T)



car_dem <- rast_terrain[[1]]
# layers_mapa[["Ecosistemas - IDEAM año 2017"]]$COBERTURA %>% unique()


if(!is.null(sel_municipios)){

  municipios_crop <- layers_mapa[["Municipios"]] %>% 
    filter(Municipio %in% sel_municipios)
  
  municipio_dem <-  crop(car_dem, vect(municipios_crop), mask = T)
  
  
  layers_mapa <- layers_mapa %>%
    map(~st_make_valid(.x) %>% 
          st_intersection(municipios_crop["Municipio"]))
  
  
  layers_eventos_dgoat <- st_intersection(st_transform(layers_eventos_dgoat, crs = 4326), municipios_crop["Municipio"])
  
  
}


# plot(car_dem)
# plot(municipio_dem)



## Layers del mapa

#3 Eventos de incendios, ubicacion

eventos_criticos <- cleaning_puntos_criticos(
  layers_eventos_dgoat,
  clase = "Incendio Forestal") %>% st_transform(., crs = 4326)


municipio <- layers_mapa[["Municipios"]]
centro_poblado <- layers_mapa[["Centro poblado DANE"]]
veredas <- layers_mapa[["Veredas"]]
areas_protegidas <- layers_mapa[["Áreas Naturales Protegidas"]]
# cuencas <- layers_mapa[["Veredas"]]
ecosistemas <- layers_mapa[["Ecosistemas - IDEAM año 2017"]]

rios <- layers_mapa[["Drenaje Doble"]]
quebradas <- layers_mapa[["Drenaje sencillo"]]
embalses <- layers_mapa[["Embalse"]]
vias <- layers_mapa[["Vía"]] %>% filter(TIPO_VIA  %in% c(1:4))
# 
# 

eventos_criticos

eventos_criticos %>%
  ggplot(aes(x = factor(Año))) +
  geom_bar(color = "black", fill = "gray") +
  labs(title = "Distribución de incendios por año", x = "Año", y = "Cantidad de incendios") +
  theme_minimal()

eventos_criticos %>% drop_na(`Causas_Tipos`) %>%
  group_by(`Causas_Tipos`) %>%
  summarise(total_area = sum(area_hectareas, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(`Causas_Tipos`, total_area), y = total_area)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Área quemada por tipo de incendio", x = "Causas Tipos", y = "Área quemada (ha)") +
  theme_minimal() 

eventos_criticos %>% drop_na(Categoria) %>%
  group_by(Categoria) %>%
  summarise(total_area = sum(area_hectareas, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Categoria, total_area), y = total_area)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Área quemada por Categoria de incendio", x = "Categoria", y = "Área quemada (ha)") +
  theme_minimal() 


eventos_criticos %>% drop_na(`Causas_Tipos`) %>%
  ggplot(aes(x = factor(Año), fill = `Causas_Tipos`)) +
  geom_bar(color = "black") +
  labs(title = "Distribución de incendios por año", x = "Año", y = "Cantidad de incendios") +
  theme_minimal()


# Mapa contexto
ggplot(data = jurisdiccion_car) +
  geom_sf() + 
  geom_sf_text(aes(label = Direccion)) #+geom_sf(data = municipio, fill = "red")
   


ggplot() +
  geom_sf(data = areas_protegidas, fill = "darkgreen") +
  geom_sf(data = veredas %>% 
            filter(NOMBRE_VER == "CASCO URBANO"), fill = "brown") +
  geom_sf(data = areas_protegidas, fill = "darkgreen") +
  geom_sf(data = veredas, color = "darkgrey", fill = NA) +
  geom_sf(data = eventos_criticos, color = "black", size = 2, shape = 15) +
  geom_sf(data = municipios, color = "red", linewidth = 1, fill = NA) + 
#  geom_sf_text(data = st_centroid(veredas), aes(label = NOMBRE_VER), size = 2.5) +
#  geom_sf_label(areas_protegidas, aes(label = NOMBRE)) +
#  geom_sf(data = centro_poblado, fill = "brown") +
  theme_minimal()


# Paleta de colores de areas quemadas
pal_area <- colorNumeric(
  palette = colorRampPalette(c("yellow", "orange", "red", "darkred"))(256),
  domain = layers_mapa[["Riesgos - Registros de eventos"]]$area_hectareas,
  na.color = "transparent"
)

# Crear una paleta de colores categórica para "Dirección"
# pal_direccion <- colorFactor(
#   palette = brewer.pal(n = length(unique(jurisdiccion_car$Direccion)), "Set1"),
#   domain = jurisdiccion_car$Direccion
# )

pal_direccion <- colorFactor(
  palette = viridis(length(unique(jurisdiccion_car$Direccion)), option = "cividis", begin = 0.2, end = 0.8),
  domain = jurisdiccion_car$Direccion
)

pal_municipios <- colorFactor(
  palette = brewer.pal(n = length(unique(municipio)), "Set1"),
  domain = municipio$Municipio)

pal_veredas <- colorFactor(
  palette = viridis(n = length(unique(veredas)), option = "cividis"),
  domain = veredas$NOMBRE_VER)



# coberturas
# Paleta basada en CORINE Land Cover (valores RGB convertidos a hexadecimal)
paleta_clc <- c(
  "Territorio artificializado"                = "#E6004D",  # CLC 111 (230,0,77) :contentReference[oaicite:0]{index=0}
  "Bosque denso alto"                         = "#00A600",  # CLC 312 (0,166,0) :contentReference[oaicite:1]{index=1}
  "Bosque denso bajo"                         = "#80FF00",  # CLC 311 (128,255,0) :contentReference[oaicite:2]{index=2}
  "Bosque abierto alto"                       = "#4DFF00",  # CLC 313 (77,255,0) :contentReference[oaicite:3]{index=3}
  "Bosque abierto bajo"                       = "#4DFF00",  # CLC 313 (77,255,0) :contentReference[oaicite:4]{index=4}
  "Bosque de galeria y ripario"               = "#80FF00",  # CLC 311 (128,255,0) :contentReference[oaicite:5]{index=5}
  "Bosque fragmentado con pastos y cultivos"  = "#4DFF00",  # CLC 313 (77,255,0) :contentReference[oaicite:6]{index=6}
  "Bosque fragmentado con vegetacion secundaria" = "#4DFF00",# CLC 313 (77,255,0) :contentReference[oaicite:7]{index=7}
  "Vegetacion secundaria"  = "#A6F668",
  "Arbustal denso"                            = "#A6F200",  # CLC 324 (166,242,0) :contentReference[oaicite:8]{index=8}
  "Arbustal abierto"                          = "#A6E64D",  # CLC 323 (166,230,77) :contentReference[oaicite:9]{index=9}
  "Herbazal denso"                            = "#CCF24D",  # CLC 321 (204,242,77) :contentReference[oaicite:10]{index=10}
  "Herbazal abierto"                          = "#A6FF80",  # CLC 322 (166,255,128) :contentReference[oaicite:11]{index=11}
  "Areas abiertas sin vegetacion"             = "#CCFFCC",  # CLC 333 (204,255,204) :contentReference[oaicite:12]{index=12}
  "Pastos"                                    = "#E6E64D",  # CLC 231 (230,230,77) :contentReference[oaicite:13]{index=13}
  "Cultivos permanentes"                      = "#F2A64D",  # CLC 222 (242,166,77) :contentReference[oaicite:14]{index=14}
  "Papa"                                      = "#FFFFA8",  # CLC 211 (255,255,168) :contentReference[oaicite:15]{index=15}
  "Caña"                                      = "#FFFFA8",  # CLC 211 (255,255,168) :contentReference[oaicite:16]{index=16}
  "Arroz"                                     = "#E6E600",  # CLC 213 (230,230,0) :contentReference[oaicite:17]{index=17}
  "Cultivos transitorios"                     = "#FFFFA8",  # CLC 211 (255,255,168) :contentReference[oaicite:18]{index=18}
  "Cafe"                                      = "#F2A64D",  # CLC 222 (242,166,77) :contentReference[oaicite:19]{index=19}
  "Mosaico de cultivos con espacios naturales" = "#FFE6A6", # CLC 241 (255,230,166) :contentReference[oaicite:20]{index=20}
  "Mosaico de cultivos y espacios naturales" = "#FFE6A6",
  "Mosaico de cultivos y pastos"              = "#FFE64D",  # CLC 242 (255,230,77) :contentReference[oaicite:21]{index=21}
  "Mosaico de cultivos, pastos y espacios naturales" = "#E6CC4D", # CLC 243 (230,204,77) :contentReference[oaicite:22]{index=22}
  "Mosaico de pastos con espacios naturales"   = "#E6E64D", # CLC 231 (230,230,77) :contentReference[oaicite:23]{index=23}
  "Zonas pantanosas"                          = "#A6A6FF",  # CLC 411 (166,166,255) :contentReference[oaicite:24]{index=24}
  "Mosaico de pastos y espacios naturales"   = "#E6E64D",
  "Turberas"                                   = "#4D4DFF",  # CLC 412 (77,77,255) :contentReference[oaicite:25]{index=25}
  "Vegetacion acuatica sobre cuerpos de agua" = "#80F2E6",  # CLC 512 (128,242,230) :contentReference[oaicite:26]{index=26}
  "Rio"                                       = "#00CCF2",  # CLC 511 (0,204,242) :contentReference[oaicite:27]{index=27}
  "Canales"                                   = "#00CCF2",  # CLC 511 (0,204,242) :contentReference[oaicite:28]{index=28}
  "Cuerpo de agua artificial"                 = "#80F2E6",  # CLC 512 (128,242,230) :contentReference[oaicite:29]{index=29}
  "Laguna"                                    = "#00FFA6",  # CLC 521 (0,255,166) :contentReference[oaicite:30]{index=30}
  "Zonas arenosas naturales"                  = "#E6E6E6",  # CLC 331 (230,230,230) :contentReference[oaicite:31]{index=31}
  "Afloramientos rocosos"                     = "#CCCCCC",  # CLC 332 (204,204,204) :contentReference[oaicite:32]{index=32}
  "Plantacion forestal"                       = "#00A600",  # CLC 312 (0,166,0) :contentReference[oaicite:33]{index=33}
  "Nubes"                                     = "#FFFFFF"   # NoData/Clouds default (blanco) :contentReference[oaicite:34]{index=34}
)


# 1. Forzar factor con niveles ordenados
niveles_clc <- names(paleta_clc)
ecosistemas$COBERTURA <- factor(ecosistemas$COBERTURA, levels = niveles_clc)

# 2. Crear paleta para Leaflet
paleta_clc_map <- colorFactor(
  palette = paleta_clc,
  domain  = niveles_clc,
  levels  = niveles_clc
)

# 3. Verificar que 'Rio' coincide:
# paleta_clc_map("Vegetacion secundaria") 
# "#A6F668"

# Ver la paleta
# Si ya tienes paleta_clc con nombres idénticos a categorias
# paleta_clc_map <- function(cobertura){
#   
#   paleta_clc <- c(
#     "Territorio artificializado"                = "#E6004D",  # CLC 111: Continuous urban fabric (230,0,77) :contentReference[oaicite:3]{index=3}
#     "Bosque denso alto"                         = "#00A600",  # CLC 312: Coniferous forest (0,166,0) :contentReference[oaicite:4]{index=4}
#     "Bosque denso bajo"                         = "#80FF00",  # CLC 311: Broad-leaved forest (128,255,0) :contentReference[oaicite:5]{index=5}
#     "Bosque abierto alto"                       = "#4DFF00",  # CLC 313: Mixed forest (77,255,0) :contentReference[oaicite:6]{index=6}
#     "Bosque abierto bajo"                       = "#4DFF00",  # CLC 313: Mixed forest (77,255,0) :contentReference[oaicite:7]{index=7}
#     "Bosque de galeria y ripario"               = "#80FF00",  # CLC 311: Broad-leaved forest (128,255,0) :contentReference[oaicite:8]{index=8}
#     "Bosque fragmentado con pastos y cultivos"  = "#4DFF00",  # CLC 313: Mixed forest (77,255,0) :contentReference[oaicite:9]{index=9}
#     "Bosque fragmentado con vegetación secundaria" = "#4DFF00",  # CLC 313: Mixed forest (77,255,0) :contentReference[oaicite:10]{index=10}
#     "Arbustal denso"                            = "#A6F200",  # CLC 324: Transitional woodland-shrub (166,242,0) :contentReference[oaicite:11]{index=11}
#     "Arbustal abierto"                          = "#A6E64D",  # CLC 323: Sclerophyllous vegetation (166,230,77) :contentReference[oaicite:12]{index=12}
#     "Herbazal denso"                            = "#CCF24D",  # CLC 321: Natural grasslands (204,242,77) :contentReference[oaicite:13]{index=13}
#     "Herbazal abierto"                          = "#A6FF80",  # CLC 322: Moors and heathland (166,255,128) :contentReference[oaicite:14]{index=14}
#     "Pastos"                                    = "#E6E64D",  # CLC 231: Pastures (230,230,77) :contentReference[oaicite:15]{index=15}
#     "Cultivos permanentes"                      = "#F2A64D",  # CLC 222: Fruit trees and berry plantations (242,166,77) :contentReference[oaicite:16]{index=16}
#     "Papa"                                      = "#FFFFA8",  # CLC 211: Non-irrigated arable land (255,255,168) :contentReference[oaicite:17]{index=17}
#     "Caña"                                      = "#FFFFA8",  # CLC 211: Non-irrigated arable land (255,255,168) :contentReference[oaicite:18]{index=18}
#     "Arroz"                                     = "#E6E600",  # CLC 213: Rice fields (230,230,0) :contentReference[oaicite:19]{index=19}
#     "Cultivos transitorios"                     = "#FFFFA8",  # CLC 211: Non-irrigated arable land (255,255,168) :contentReference[oaicite:20]{index=20}
#     "Cafe"                                      = "#F2A64D",  # CLC 222: Fruit trees and berry plantations (242,166,77) :contentReference[oaicite:21]{index=21}
#     "Mosaico de cultivos con espacios naturales" = "#FFE6A6",  # CLC 241: Annual crops associated with permanent crops (255,230,166) :contentReference[oaicite:22]{index=22}
#     "Mosaico de cultivos y pastos"              = "#FFE64D",  # CLC 242: Complex cultivation patterns (255,230,77) :contentReference[oaicite:23]{index=23}
#     "Mosaico de cultivos, pastos y espacios naturales" = "#E6CC4D",  # CLC 243: Land principally occupied by agriculture with significant areas of natural vegetation (230,204,77) :contentReference[oaicite:24]{index=24}
#     "Mosaico de pastos con espacios naturales"   = "#E6E64D",  # CLC 231: Pastures (230,230,77) :contentReference[oaicite:25]{index=25}
#     "Mosaico de cultivos y espacios naturales"   = "#FFE6A6",  # CLC 241: Annual crops associated with permanent crops (255,230,166) :contentReference[oaicite:26]{index=26}
#     "Mosaico de pastos y espacios naturales"     = "#E6CC4D",  # CLC 243: Land principally occupied by agriculture with significant areas of natural vegetation (230,204,77) :contentReference[oaicite:27]{index=27}
#     "Zonas pantanosas"                          = "#A6A6FF",  # CLC 411: Inland marshes (166,166,255) :contentReference[oaicite:28]{index=28}
#     "Turberas"                                   = "#4D4DFF",  # CLC 412: Peat bogs (77,77,255) :contentReference[oaicite:29]{index=29}
#     "Vegetación acuatica sobre cuerpos de agua"  = "#80F2E6",  # CLC 512: Water bodies (128,242,230) :contentReference[oaicite:30]{index=30}
#     "Rio"                                       = "#00CCF2",  # CLC 511: Water courses (0,204,242) :contentReference[oaicite:31]{index=31}
#     "Canales"                                   = "#00CCF2",  # CLC 511: Water courses (0,204,242) :contentReference[oaicite:32]{index=32}
#     "Cuerpo de agua artificial"                 = "#80F2E6",  # CLC 512: Water bodies (128,242,230) :contentReference[oaicite:33]{index=33}
#     "Laguna"                                    = "#00FFA6",  # CLC 521: Coastal lagoons (0,255,166) :contentReference[oaicite:34]{index=34}
#     "Zonas arenosas naturales"                  = "#E6E6E6",  # CLC 331: Beaches, dunes, sands (230,230,230) :contentReference[oaicite:35]{index=35}
#     "Afloramientos rocosos"                     = "#CCCCCC"   # CLC 332: Bare rocks (204,204,204) :contentReference[oaicite:36]{index=36}
#   )
#   
#   paleta_clc[[cobertura]]
#   
#   
# }


# "#E6E64D"  "#A6FF80" "#80F2E6"
# paleta_clc_map("Canales")
# Rio esta definido como : "#00CCF2" 
# Crear paleta de colores para la elevación


pal_elevacion <- colorNumeric(
  palette = terrain.colors(256),  # Paleta para la elevación
  domain = values(municipio_dem),      # Rango de valores del raster DEM
  na.color = "transparent"
)






# Crear el mapa interactivo con múltiples mapas base
mapa_dyn <-leaflet() %>%
  # Mapas base
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
  #addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  
#Agregar el DEM reproyectado
 addRasterImage(municipio_dem, colors = pal_elevacion, opacity = 0.7, group = "Elevación") %>%
  
  # Agregar límites de jurisdicción reproyectados
  addPolygons(data = jurisdiccion_car, color = "black", weight = 1, 
              fillOpacity = 0.6,
              fillColor = ~pal_direccion(Direccion), 
              group = "Jurisdicción CAR", 
              popup = ~paste0("<strong>Dirección:</strong> ", Direccion)) %>%
  addPolygons(data = municipio, color = "black", weight = 1, 
              fillOpacity = 0.6,
              fillColor = ~pal_municipios(Municipio), 
              group = "Municipios", 
              popup = ~paste0("<strong>Municipios:</strong> ", Municipio)) %>%
  addPolygons(data = veredas, color = "green", weight = 1, 
              fillOpacity = 0.0,
              fillColor = ~pal_veredas(NOMBRE_VER), 
              group = "Veredas", 
              popup = ~paste0("<strong>Vereda:</strong> ", NOMBRE_VER)) %>%
  addPolygons(data = ecosistemas, color = "gray", weight = 1, 
              fillOpacity = 0.3,
              fillColor = ~paleta_clc_map(COBERTURA), 
              group = "Cobertura", 
              popup = ~paste0("<strong>Cobertura:</strong> ", COBERTURA)) %>%
  
  # Agregar polígono veredas reproyectado
  addPolylines(data = layers_mapa[["Drenaje Doble"]], color = "blue", weight = 2,
               group = "Rios", 
               popup = ~paste0("<strong>Nombre:</strong> ", NOMBRE_GEOGRAFICO)) %>%
  
  # Agregar puntos de incendios con popups
  addCircleMarkers(data = eventos_criticos,  #%>% mutate(  Mes = month(Fecha_reporte, label = T)),
                   radius = 3,  # Tamaño proporcional al área afectada
                   color = "red" ,  #~pal_area(area_hectareas),  # Color basado en el área
                   fillColor = "black", #~pal_area(area_hectareas),
                   fillOpacity = 0.7, stroke = FALSE,
                   group = "Registro Incendios",
                   popup = ~paste0(
                     "<strong>Municipio:</strong> ", Municipio, "<br>",
                     "<strong>Vereda Localidad:</strong> ", `Vereda_Localidad`, "<br>",
                     "<strong>Año de Registro:</strong> ", `Año`, "<br>",
                    # "<strong>Mes de Registro:</strong> ", Mes, "<br>",
                     # "<strong>Nombre o Punto:</strong> ", `Nombre Punto`, "<br>",
                     "<strong>Área Afectada (ha):</strong> ", round(area_hectareas, 2), "<br>",
                     "<strong>Categoría:</strong> ", Categoria, "<br>",
                     "<strong>Causa o Tipo:</strong> ", `Causas_Tipos`
                   )) %>%
  
  # Agregar leyenda para el área afectada
  # addLegend("bottomright", pal = pal_area, values = eventos_criticos$area_hectareas,
  #           title = "Área Afectada (ha)", opacity = 1) %>%
  
  # Agregar leyenda para la elevación
  addLegend("bottomleft", pal = pal_elevacion, values = values(municipio_dem),
            title = "Elevación (m)", opacity = 1) %>%
  
  # Agregar control de capas
  addLayersControl(
    baseGroups = c("OpenStreetMap", "CartoDB Positron", "Esri World Imagery"),
    overlayGroups = c("Jurisdicción CAR","Municipios", "Veredas", 
                      "Cobertura", "Elevación", 
                      "Registro Incendios", "Rios"),# "POMCA Río Bogotá", "Elevación"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  
  # Ajustar la vista del mapa a los datos
  setView(lng = mean(st_coordinates(eventos_criticos)[,1]), 
          lat = mean(st_coordinates(eventos_criticos)[,2]), zoom = 8)


# 2. Guardar como HTML autosuficiente
saveWidget(
  widget     = mapa_dyn,
  file       = paste0("mapas/mapa_leaflet_", sel_municipios, ".html")
  selfcontained = TRUE, 
)






