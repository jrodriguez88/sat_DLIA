## Mapa para Ploteo


# --------------- 1. Librerías necesarias ---------------
# library(sf)        # manejo de datos vectoriales
# library(tmap)      # mapeo temático
# library(RColorBrewer) # paletas de colores

# Establecer modo para gráfico estático (ideal para impresión)
tmap_mode("plot")

# Paleta en tonos rojos para TIPOS de vía
pal_rojos <- brewer.pal(n = length(unique(vias$TIPO_VIA)), "Reds")


# --------------- 4. Construir el mapa con tmap ---------------
# --------------- 4. Construir el mapa con tmap ---------------
mapa_final <- tm_shape() +
  # 4.1. Mapa base (tiles)
  tm_tiles("CartoDB.Positron", alpha = 0.6) +
  
  # 4.2. Capas de contexto
  # 4.2.1. Ecosistemas (relleno semitransparente según COBERTURA)
  tm_shape(ecosistemas) +
  tm_fill(col     = "COBERTURA",
          palette = paleta_clc,
          title   = "Ecosistemas (2017)",
          alpha   = 0.8) +
  tm_borders(col = "gray50", lwd = 0.3) +
  
  # 4.2.2. Áreas Naturales Protegidas (y entrada en leyenda)
  tm_shape(areas_protegidas) +
  tm_polygons(col        = "lightgreen",
              border.col = "darkgreen",
              lwd        = 2,
              alpha      = 0.4,
              title      = "Áreas protegidas") +
  
  # 4.2.3. Veredas (línea fina) y sus etiquetas (NOMBRE_VER)
  tm_shape(veredas) +
  tm_lines(col  = "brown",
           lty  = 2,
           lwd  = 2,
           title = "Veredas") +
  tm_text(text      = "NOMBRE_VER",
          size      = 1,        # Ajustar tamaño de fuente
          col       = "black",
          remove.overlap = TRUE,  # Evita superposición de etiquetas
          shadow    = FALSE,
          just      = "left") +
  
  # 4.2.4. Centros poblados (filtrando la “vereda” que corresponda a Casco Urbano)
  tm_shape(veredas %>% filter(NOMBRE_VER == "CASCO URBANO")) +
  tm_polygons(col        = "#E31A1C",
              border.col = "darkred",
              lwd        = 1.5,
              alpha      = 0.8,
              title      = "Casco Urbano") +
  
  # 4.3. Hidrografía
  # 4.3.1. Ríos principales
  tm_shape(rios) +
  tm_lines(col   = "#2171B5",
           lwd   = 2,
           title = "Ríos") +
  
  # 4.3.2. Quebradas (línea fina)
  tm_shape(quebradas) +
  tm_lines(col   = "#6BAED6",
           lwd   = 0.4,
           title = "Quebradas") +
  
  # 4.3.3. (Opcional) Embalses
  # tm_shape(embalses) +
  #   tm_fill(col   = "#6BAED6",
  #           alpha = 0.5,
  #           title = "Embalses") +
  #   tm_borders(col = "#2171B5", lwd = 0.5) +
  
  # 4.4. Infraestructura vial: color según TIPOS de vía en paleta de rojos
  tm_shape(vias) +
  tm_lines(col     = "TIPO_VIA",
           palette = rev(pal_rojos),
           lwd     = 2,
           title   = "Tipo de vía") +
  # 4.5. Eventos críticos (puntos como cuadrados sólidos)
  tm_shape(eventos_criticos) +
  tm_symbols(shape      = 22,   # ó shape = 22 para cuadrados :contentReference[oaicite:2]{index=2}
             size       = 1,        # tamaño relativo (ajustable según escala)
             col        = "black",    # color de relleno del cuadrado
             border.col = NA,         # sin borde (puedes cambiar a "gray20" si prefieres contorno)
             alpha      = 1.0,        # totalmente opaco
             title      = "Eventos críticos") +
  tm_add_legend(
    type   = "line",
    labels = c("Limite Veredal", "Ríos", "Quebradas"),
    col    = c("brown",   "#2171B5", "#6BAED6"),
    lwd    = c(1.5,      2, 1 ),
    lty    = c(2,        1, 1),
    title  = ""
  ) +
  
  tm_add_legend(
    type       = "fill",
    labels     = c("Áreas protegidas", "Casco Urbano"),
    col        = c("#90EE90", "#E31A1C"),      # lightgreen en hex
    border.col = c("#006400", "darkred"),      # darkgreen en hex
    alpha      = c(0.4, 0.8), 
    border.lwd = c(2, 1.5),
    title      = ""
  ) + 
  
  # 4.5. Cuadrícula (graticule)
  tm_grid(lines                = TRUE,
          n.x                  = 10,
          n.y                  = 10,
          col                  = "gray90",
          lwd                  = 0.2,
          labels.inside.frame  = FALSE,
          labels.size          = 0.7) +
  
  # 4.6. Elementos de layout: título, márgenes, fondo de leyenda, etc.
  tm_layout(
    title                   = paste0("Municipio de ", str_to_title(sel_municipios)),
    title.position          = c("left", "top"),
    title.size              = 5.0,        # equivale aprox. a 24 pt
    title.fontface          = "bold",
    title.bg.color  = "white",
    
    legend.outside          = FALSE,
    legend.position = c("right", "top"),
    legend.title.size       = 1.2,        # equivale aprox. a 14–16 pt
    legend.text.size        = 1.0,        # equivale aprox. a 12–14 pt
    legend.bg.color         = "white",
    legend.bg.alpha         = 0.7,
    
    frame                   = TRUE,
    frame.lwd               = 1.0,
    frame.col               = "black",
    
    outer.margins           = c(0.02, 0.02, 0.02, 0.02),
    inner.margins           = c(0.01, 0.01, 0.01, 0.01)
  ) +
  
  # 4.7. Flecha Norte
  tm_compass(size     = 5.0,
             position = c("left", "top")) +
  
  # 4.8. Barra de escala
  tm_scale_bar(position   = c("left", "bottom"),
               text.size  = 1.2,           # equivale aprox. a 14–16 pt
               breaks     = c(0, 1, 2, 5, 10),
               text.color = "black",
               color.dark = "black",
               color.light = "white"
  ) +
  tm_credits(
    text = paste(
      "Fuente de datos: CAR https://sig.car.gov.co/arcgis/rest/services/VISOR",
      "Autor: Jeferson Rodríguez-Espinoza",
      "Fecha: Jun-2025",
      "Sistema de referencia: WGS 84 (EPSG:4326)",
      sep = "\n"
    ),
    size       = 1.0,
    color      = "black",
    fontface   = "plain",
    bg.color   = "white",
    bg.alpha   = 0.7,
    frame      = TRUE,
    frame.lwd  = 0.5,
    frame.r    = 0.1,
    just       = "left",
    position   = c("left", "bottom"),
    padding    = 0.5
  )

# --------------- 5. Visualizar el mapa en R ---------------
mapa_final

# 3.1. Mapa de contexto con tmap
mapa_contexto <- tm_shape(jurisdiccion_car) +
  tm_polygons(col  = "gray90",   # gris muy pálido
              border.col = "gray60",
              lwd  = 0.5,
              alpha = 1.0,
              title = "Jurisdicción CAR") +
  tm_shape(municipio) +
  tm_polygons(col  = "red",      # municipio en rojo
              border.col = "darkred",
              lwd  = 0.8,
              alpha = 1.0,
              title = "Municipio") +
  tm_layout(
    title = "Ubicacíon del Municipio en el territorio CAR",
    frame = TRUE,               # sin marco para el inset
    legend.show = FALSE,         # sin leyenda en el inset
    inner.margins = c(0.01,0.01,0.1,0.01)
  )

# grid.newpage()  
# 
# # 4.3. Imprimir el mapa principal
# print(mapa_final)
# 
# # 4.4. Definir un viewport para el inset (ejemplo: corner = bottom-left, ancho = 0.25, alto = 0.25)
# #     Los valores están en proporción 0–1 respecto al dispositivo gráfico actual.
# #     Aquí: x = 0.15 (15% desde el borde izquierdo), y = 0.15 (15% desde el borde inferior)
# #     width = 0.25 (25% del ancho total), height = 0.25 (25% del alto total)
# mi_vp <- viewport(x      = 0.25,
#                   y      = 0.15,
#                   width  = 0.25,
#                   height = 0.25,
#                   just   = c("left", "bottom"))
# 
# # 4.5. Imprimir el mapa de contexto dentro del viewport “mi_vp”
# print(mapa_contexto, vp = mi_vp)


# 5.1. Guardar en PDF A0 (orientación horizontal: 118.9 cm × 84.1 cm)
pdf(paste0("mapa_", sel_municipios, "_A0.pdf"),
    width  = 84.1  / 2.54,   # convertir cm a pulgadas
    height = 118.9 / 2.54,
    family = "Helvetica")    # familia de fuente opcional

# 5.2. Repetimos el proceso de impresión con viewport dentro del PDF
print(mapa_final)
mi_vp <- viewport(x      = 0.795,
                  y      = 0.14,
                  width  = 0.20,
                  height = 0.20,
                  just   = c("left", "bottom"))
print(mapa_contexto, vp = mi_vp)

# 5.3. Cerrar el dispositivo PDF
dev.off()


# sel_municipios
# tmap_save(mapa_final,
#           filename = paste0("mapa_", sel_municipios, "_A0.pdf"),
#           width = 118.9,
#           height = 84.1,
#           units = "cm",
#           dpi = 300)





