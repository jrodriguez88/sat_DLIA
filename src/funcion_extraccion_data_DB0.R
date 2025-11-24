# Requiere: sf (>=1.0), terra (>=1.7), dplyr
# library(sf)
# library(terra)
# library(dplyr)

# eventos_dentro_cuenca
# clu_db

# ──────────────────────────────────────────────────────────────────────────────
# Utilidades
modo_simple <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# focal sin centro (anillo Moore) para numeric
.moore_mean <- function(v) {
  if (!length(v)) return(NA_real_)
  i0 <- ceiling(length(v)/2)
  mean(v[-i0], na.rm = TRUE)
}
# focal sin centro (anillo Moore) para categórico
.moore_mode <- function(v) {
  if (!length(v)) return(NA)
  i0 <- ceiling(length(v)/2)
  modo_simple(v[-i0])
}

aw_mean <- function(values, areas) { # promedio ponderado por área
  if (!length(values)) return(NA_real_)
  sA <- sum(as.numeric(areas), na.rm = TRUE)
  if (isTRUE(all.equal(sA, 0))) return(NA_real_)
  sum(as.numeric(values) * as.numeric(areas), na.rm = TRUE) / sA
}
aw_mode <- function(values, areas) { # modo ponderado por área
  if (!length(values)) return(NA)
  df <- data.frame(val = values, w = as.numeric(areas)) |>
    dplyr::group_by(val) |>
    dplyr::summarise(w = sum(w, na.rm = TRUE), .groups = "drop")
  df$val[which.max(df$w)]
}

# ──────────────────────────────────────────────────────────────────────────────
# Función principal
extrae_estadisticas_espaciales <- function(
    geom_extract,               # sf POINT o POLYGON/MULTIPOLYGON
    base,                       # SpatRaster o sf (polígonos)
    campo_base = NULL,          # nombre de columna en base vectorial (si NULL usa geometría, ver notas)
    id_col = NULL,              # columna id en geom_extract (si NULL usa rowid)
    radio_celdas = 1,           # radio Moore para raster (1 = ventana 3x3)
    buffer_dist = NULL,         # opcional para polígonos+raster: distancia del anillo si no quieres usar pixel-size
    tratar_como = c("auto","numerico","categorico"), # forzar tipo cuando sea ambiguo
    na.rm = TRUE
) {
  tratar_como <- match.arg(tratar_como)
  
  # id
  if (is.null(id_col)) {
    geom_extract <- geom_extract |> dplyr::mutate(.oid = dplyr::row_number())
    id_col <- ".oid"
  } else if (!id_col %in% names(geom_extract)) stop("id_col no existe en geom_extract")
  
  # familia de casos
  is_raster <- inherits(base, "SpatRaster")
  is_vec    <- inherits(base, "sf")
  
  if (!is_raster && !is_vec) stop("`base` debe ser terra::SpatRaster o sf (polígonos).")
  
  # geom tipo
  gtype <- unique(as.character(sf::st_geometry_type(geom_extract)))
  if (!all(gtype %in% c("POINT","MULTIPOINT","POLYGON","MULTIPOLYGON"))) {
    stop("geom_extract debe ser POINT/MULTIPOINT o POLYGON/MULTIPOLYGON")
  }
  is_points <- all(gtype %in% c("POINT","MULTIPOINT"))
  
  # ───────── Alineación CRS
  if (is_raster) {
    # llevar geom al CRS del raster
    geom_use <- sf::st_transform(geom_extract, terra::crs(base))
  } else {
    # vector-vector: llevar base al CRS del geom
    if (sf::st_crs(geom_extract)$epsg != sf::st_crs(base)$epsg) {
      base <- sf::st_transform(base, sf::st_crs(geom_extract))
    }
    geom_use <- geom_extract
  }
  
  # Detectar tipo (num/cat) si "auto"
  tipo_auto <- function(x) {
    if (inherits(x, "SpatRaster")) {
      # Si tiene niveles de categoría definidos es categórico
      has_levels <- length(terra::levels(x)) > 0
      if (has_levels) return("categorico")
      # Si es entero con pocos valores únicos puede ser categoría,
      # pero evitar muestrear todo: toma una muestra
      v <- terra::values(x, mat = FALSE, na.rm = TRUE)[seq(1, terra::ncell(x), length.out = min(5000, terra::ncell(x)))]
      if (is.integer(v) || (is.numeric(v) && length(unique(v)) <= 20)) return("categorico")
      return("numerico")
    } else {
      if (is.null(campo_base)) return("geometria")
      if (is.numeric(base[[campo_base]]) || is.integer(base[[campo_base]])) return("numerico")
      return("categorico")
    }
  }
  tipo <- if (tratar_como == "auto") tipo_auto(base) else tratar_como
  
  # ────────────────────────────────────────────────────────────────────────────
  # Casos RASTER
  if (is_raster && is_points) {
    # puntos + raster
    nm <- names(base)[1]
    # raster focal (anillo Moore) según tipo
    k <- 2*radio_celdas + 1
    if (tipo == "categorico") {
      r_nei <- terra::focal(base, w = matrix(1, k, k), fun = "modal", na.rm = TRUE) %>% 
        setNames(nm)
    } else {
      r_nei <- terra::focal(base, w = matrix(1, k, k), fun = "mean", na.rm = TRUE)%>% 
        setNames(nm)
    }
    # extraer
    v0 <- terra::extract(base, terra::vect(geom_use))[, nm, drop = TRUE]
    vn <- terra::extract(r_nei, terra::vect(geom_use))[, nm, drop = TRUE]
    out <- geom_use |>
      sf::st_drop_geometry() |>
      dplyr::select(all_of(id_col)) |>
      dplyr::mutate(
        valor     = v0,
        vecindad  = vn
      )
    return(out)
  }
  
  if (is_raster && !is_points) {
    # polígonos + raster
    # valor: intersección exacta (mean o modo)
    if (tipo == "categorico") {
      # extraemos todas las celdas por polígono y calculamos modo
      df_vals <- terra::extract(base, terra::vect(geom_use))  # ID, layer
      nm <- names(base)[1]
      val_df <- df_vals |>
        dplyr::rename(valor_celda = !!nm) |>
        dplyr::group_by(ID) |>
        dplyr::summarise(valor = modo_simple(valor_celda), .groups = "drop")
    } else {
      nm <- names(base)[1]
      val_df <- terra::extract(base, terra::vect(geom_use), fun = mean, na.rm = na.rm, exact = TRUE)
      val_df <- val_df |>
        dplyr::rename(valor = !!nm)
    }
    
    # vecindad: anillo (buffer - polígono). Distancia por defecto = tamaño de celda máx * radio
    if (is.null(buffer_dist)) {
      res_xy <- terra::res(base)
      buffer_dist <- max(res_xy) * radio_celdas
    }
    ring_geom <- sf::st_buffer(geom_use, dist = buffer_dist)
    ring_only <- sf::st_difference(ring_geom, sf::st_union(geom_use)) # área anular
    
    # por si geometrías inválidas
    suppressWarnings(ring_only <- sf::st_make_valid(ring_only))
    
    if (tipo == "categorico") {
      df_ring <- terra::extract(base, terra::vect(ring_only))
      df_ring <- df_ring |>
        dplyr::rename(val = !!nm) |>
        dplyr::group_by(ID) |>
        dplyr::summarise(vecindad = modo_simple(val), .groups = "drop")
    } else {
      df_ring <- terra::extract(base, terra::vect(ring_only), fun = mean, na.rm = na.rm, exact = TRUE)
      df_ring <- df_ring |>
        dplyr::rename(vecindad = !!nm)
    }
    
    res <- geom_use |>
      sf::st_drop_geometry() |>
      dplyr::select(all_of(id_col)) |>
      dplyr::mutate(ID = dplyr::row_number()) |>
      dplyr::left_join(val_df, by = "ID") |>
      dplyr::left_join(df_ring, by = "ID") |>
      dplyr::select(all_of(id_col), valor, vecindad)
    return(res)
  }
  
  # ────────────────────────────────────────────────────────────────────────────
  # Casos VECTOR (base = sf POLÍGONOS)
  if (is_vec && is_points) {
    if (is.null(campo_base)) {
      # valor: id del polígono contenedor; vecindad: id más frecuente de vecinos (queen)
      idx <- sf::st_within(geom_use, base)
      cont_id <- vapply(idx, function(ii) if (length(ii)) ii[1] else NA_integer_, integer(1))
      
      neigh_list <- sf::st_touches(base)
      neigh_ids <- vapply(cont_id, function(ci) {
        if (is.na(ci)) return(NA_integer_)
        nn <- neigh_list[[ci]]
        if (!length(nn)) return(NA_integer_)
        modo_simple(nn)  # id vecino más frecuente (proxy)
      }, integer(1))
      
      out <- geom_use |>
        sf::st_drop_geometry() |>
        dplyr::select(all_of(id_col)) |>
        dplyr::mutate(
          valor    = cont_id,
          vecindad = neigh_ids
        )
      return(out)
      
    } else {
      # valor: atributo del polígono contenedor
      join <- sf::st_join(geom_use, base[, c(campo_base)], join = sf::st_within, left = TRUE)
      
      # IMPORTANTE: calcular cont_id ALINEADO a 'join' para evitar desajustes de longitud
      idx_j <- sf::st_within(join, base)
      cont_id <- vapply(idx_j, function(ii) if (length(ii)) ii[1] else NA_integer_, integer(1))
      
      # vecindad: promedio/moda del atributo en polígonos vecinos del contenedor (queen)
      neigh_list <- sf::st_touches(base)
      
      is_num <- is.numeric(base[[campo_base]]) || is.integer(base[[campo_base]])
      vec_val <- vapply(cont_id, function(ci) {
        if (is.na(ci)) return(if (is_num) NA_real_ else NA_character_)
        nn <- neigh_list[[ci]]
        if (!length(nn)) return(if (is_num) NA_real_ else NA_character_)
        vals <- base[[campo_base]][nn]
        if (is_num) mean(vals, na.rm = na.rm) else modo_simple(vals)
      }, FUN.VALUE = if (is_num) numeric(1) else character(1))
      
      # (opcional) comprobación dura
      # stopifnot(length(vec_val) == nrow(join))
      
      if(campo_base %in% names(geom_extract)) {
        
        out <- join |>
          sf::st_drop_geometry() |>
          dplyr::transmute(
            !!id_col := .data[[id_col]],
            valor     = .data[[paste0(campo_base, ".y")]],
            vecindad  = vec_val
          )
        
      } else {
      
      out <- join |>
        sf::st_drop_geometry() |>
        dplyr::transmute(
          !!id_col := .data[[id_col]],
          valor     = .data[[campo_base]],
          vecindad  = vec_val
        )
      
      }
      
      return(out)
    }
  }
  
  if (is_vec && !is_points) {
    # polígonos + polígonos
    if (is.null(campo_base)) {
      # valor = % cobertura de base sobre el polígono objetivo
      # vecindad = % cobertura de base sobre el anillo exterior
      tgt <- geom_use
      inter <- sf::st_intersection(sf::st_make_valid(base), sf::st_make_valid(tgt))
      if (nrow(inter) == 0) {
        out <- tgt |>
          sf::st_drop_geometry() |>
          dplyr::select(all_of(id_col)) |>
          dplyr::mutate(valor = 0, vecindad = 0)
        return(out)
      }
      inter$area_int <- sf::st_area(inter)
      area_tgt <- sf::st_area(tgt)
      # porcentaje de cobertura dentro
      val_df <- inter |>
        dplyr::group_by(!!rlang::sym(id_col)) |>
        dplyr::summarise(area_cub = sum(area_int, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(valor = as.numeric(area_cub / area_tgt[match(.data[[id_col]], tgt[[id_col]])]))
      # anillo vecindad (buffer una distancia igual al promedio de anchura del polígono)
      if (is.null(buffer_dist)) {
        # heurística: 2% del diámetro del bbox
        bb <- sf::st_bbox(tgt)
        buffer_dist <- 0.02 * sqrt((bb["xmax"]-bb["xmin"])^2 + (bb["ymax"]-bb["ymin"])^2)
      }
      ring_geom <- sf::st_difference(sf::st_buffer(tgt, dist = buffer_dist), tgt)
      suppressWarnings(ring_geom <- sf::st_make_valid(ring_geom))
      inter_ring <- sf::st_intersection(sf::st_make_valid(base), ring_geom)
      if (nrow(inter_ring)) {
        inter_ring$area_int <- sf::st_area(inter_ring)
        area_ring <- sf::st_area(ring_geom)
        nei_df <- inter_ring |>
          dplyr::group_by(!!rlang::sym(id_col)) |>
          dplyr::summarise(area_cub = sum(area_int, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(vecindad = as.numeric(area_cub / area_ring[match(.data[[id_col]], ring_geom[[id_col]])]))
      } else {
        nei_df <- tgt |>
          sf::st_drop_geometry() |>
          dplyr::select(all_of(id_col)) |>
          dplyr::mutate(vecindad = 0)
      }
      
      out <- tgt |>
        sf::st_drop_geometry() |>
        dplyr::select(all_of(id_col)) |>
        dplyr::left_join(val_df |> dplyr::select(all_of(id_col), valor), by = id_col) |>
        dplyr::left_join(nei_df |> dplyr::select(all_of(id_col), vecindad), by = id_col)
      return(out)
    } else {
      # valor: promedio/modo ponderado por área del atributo en la intersección
      tgt <- geom_use
      inter <- sf::st_intersection(sf::st_make_valid(base), sf::st_make_valid(tgt))
      if (!nrow(inter)) {
        out <- tgt |>
          sf::st_drop_geometry() |>
          dplyr::select(all_of(id_col)) |>
          dplyr::mutate(valor = NA, vecindad = NA)
        return(out)
      }
      inter$area_int <- sf::st_area(inter)
      is_num <- is.numeric(base[[campo_base]]) || is.integer(base[[campo_base]])
      # copiar atributo
      inter$.val <- inter[[campo_base]]
      
      if (is_num) {
        val_df <- inter |>
          dplyr::group_by(!!rlang::sym(id_col)) |>
          dplyr::summarise(valor = aw_mean(.val, area_int), .groups = "drop")
      } else {
        val_df <- inter |>
          dplyr::group_by(!!rlang::sym(id_col)) |>
          dplyr::summarise(valor = aw_mode(.val, area_int), .groups = "drop")
      }
      
      # vecindad = polígonos de base que tocan el polígono objetivo (queen), fuera de la intersección
      neigh_idx_list <- sf::st_touches(base, tgt) # lista por base vs tgt
      # para cada tgt, tomar índices de base que lo tocan
      vec_vals <- sapply(seq_len(nrow(tgt)), function(i) {
        b_ids <- which(sapply(neigh_idx_list, function(v) i %in% v))
        if (!length(b_ids)) return(NA)
        vals <- base[[campo_base]][b_ids]
        if (is_num) mean(vals, na.rm = na.rm) else modo_simple(vals)
      })
      out <- tgt |>
        sf::st_drop_geometry() |>
        dplyr::select(all_of(id_col)) |>
        dplyr::left_join(val_df, by = id_col) |>
        dplyr::mutate(vecindad = vec_vals)
      return(out)
    }
  }
  
  stop("Caso no contemplado.")
}


# 
# eventos_incendios <- eventos_dentro_cuenca
# test_base_raster <- raster_clc_car[[5]]
# test_base_vectorial <- socioeconomic_data_spatial
# 
# 
# # 1) Puntos + Raster (numérico/categórico auto)
# res_pr <- extrae_estadisticas_espaciales(
#   geom_extract = eventos_incendios,
#   base = test_base_raster,        # tu SpatRaster 'base_raster'
#   id_col = NULL,
#   radio_celdas = 1            # Moore 3x3
# )
# head(res_pr)
# 
# 
# res_pr <- extrae_estadisticas_espaciales(
#   geom_extract = eventos_incendios,
#   base = rast_terrain[[1]],        # tu SpatRaster 'base_raster'
#   id_col = NULL,
#   radio_celdas = 1            # Moore 3x3
# )
# head(res_pr)
# 
# 
# # -> columnas: Nombre_Punto, valor (pixel), vecindad (Moore sin centro)
# 
# # 2) Puntos + Polígono (vector) con atributo numérico
# res_pv <- extrae_estadisticas_espaciales(
#   geom_extract = eventos_incendios,
#   base = socioeconomic_data_spatial,
#   campo_base = "Población (hab) 2025",
#   id_col = NULL
# )
# head(res_pv)
# # -> valor: población del polígono contenedor; vecindad: promedio en polígonos vecinos (queen)
# 
# 
# res_mpi0s <- extrae_estadisticas_espaciales(
#   geom_extract = eventos_incendios,
#   base = socioeconomic_data_spatial,
#   campo_base = "Municipio",
#   id_col = NULL
# )
# head(res_mpi0s)

