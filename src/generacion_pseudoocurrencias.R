# =============================================================
# 0) Paquetes y parámetros
# =============================================================

set.seed(123)

# --- Parámetros editables ---
RES_M <- 250                 # tamaño de celda / grilla (m)
BUF_M <- 200                # buffer espacial para exclusión (m)
DT_W  <- 15                  # ventana temporal (± días)
RATIO_ABS_PER_PRES <- 1      # 1 (=1:1) o 3 (=1:3)
K_TIME  <- if (RATIO_ABS_PER_PRES == 1) 1 else 1   # # ausencias time-matched por presencia
K_SPACE <- if (RATIO_ABS_PER_PRES == 1) 0 else 2   # # ausencias space-matched por presencia



# Rango temporal (mensual)
fecha_min <- min(eventos_car$Fecha)  #as.Date("2012-07-01")
fecha_max <- max(eventos_car$Fecha) 
MESES <- seq(from = floor_date(fecha_min, "month"),
             to   = floor_date(fecha_max,   "month"), by = "1 month")

# =============================================================
# 1) Preparación: CRS, columnas de apoyo y “snap” a grilla 100 m
# =============================================================
# Asegurar CRS: jurisdicción → CRS de eventos
crs_ev <- sf::st_crs(eventos_car)
juris_proj <- sf::st_transform(jurisdiccion_car, crs_ev) |> sf::st_make_valid()

# Normalizar columnas
ev <- eventos_car |>
  mutate(Fecha_Inicio = as.Date(Fecha),
         mes = floor_date(Fecha_Inicio, "month")) |> 
  # Snap a grilla 100 m vía redondeo de coords (sin crear polígonos)
  mutate(
    xy = sf::st_coordinates(geometry),
    x = xy[,1], y = xy[,2],
    cell_x = floor(x / RES_M) * RES_M,
    cell_y = floor(y / RES_M) * RES_M,
    cell_id = paste0(cell_x, "_", cell_y)
  ) |> select(-xy)

# Unir ‘Direccion’ desde la jurisdicción (si aplica)
ev <- sf::st_join(ev, juris_proj["Direccion"], left = TRUE)

# Colapso a unidad de análisis (celda 100 m × mes):
# Criterio: conservar el evento más temprano del mes por celda.
pres_cm <- ev |>
  arrange(Fecha_Inicio) |>
  group_by(cell_id, mes) |>
  slice(1) |>
  ungroup() |>
  # Reemplazar la geometría por el centro de la celda (celda 100 m)
  mutate(
    geom_cell = sf::st_as_sfc(
      mapply(function(cx, cy) sf::st_point(c(cx + RES_M/2, cy + RES_M/2)),
             cell_x, cell_y, SIMPLIFY = FALSE), crs = crs_ev
    )
  ) |>
  sf::st_set_geometry("geom_cell")

# Índices útiles
pres_keys <- pres_cm |>
  st_drop_geometry() |>
  transmute(cell_id, mes, key = paste(cell_id, mes))
PRES_KEY_SET <- unique(pres_keys$key)

# =============================================================
# 2) Utilidades
# =============================================================
# Elegir una fecha “segura” dentro de un mes, lejos (±DT_W) de fechas bloqueadas
choose_safe_date <- function(month_date, blocked_dates, window_days = DT_W){
  # Forzar clases Date y limpiar NAs
  md <- as.Date(month_date)
  if (inherits(blocked_dates, "Date")) {
    bd <- blocked_dates
  } else if (inherits(blocked_dates, "POSIXt")) {
    bd <- as.Date(blocked_dates)
  } else {
    bd <- suppressWarnings(as.Date(blocked_dates))
  }
  bd <- bd[!is.na(bd)]
  
  # Candidatas dentro del mes
  cands <- as.Date(c(
    md,
    floor_date(md, "month") + days(14),
    ceiling_date(md, "month") - days(1)
  ))
  
  for(d in cands){
    if(length(bd) == 0) return(d)
    dif <- abs(as.numeric(difftime(d, bd, units = "days")))
    if(all(dif > window_days)) return(d)
  }
  
  # Fallback: día 15 del mes
return(as.Date(floor_date(md, "month") + days(14)))
  
}



# Construir union de buffers (m) de presencias de un mes
buffers_union_for_month <- function(pres_m, buf_m = BUF_M){
  if(nrow(pres_m) == 0) return(NULL)
  sf::st_union(sf::st_buffer(sf::st_geometry(pres_m), buf_m))
}

# =============================================================
# 3) Muestreo TIME-MATCHED (mismo mes, distinto lugar)
# =============================================================
# Devuelve pseudoausencias para cada mes con K_TIME por presencia.

gen_time_matched <- function(pres_data, k_time = K_TIME){
  out_list <- vector("list", length(MESES))
  
  juris_union <- sf::st_union(juris_proj)
  
  for(i in seq_along(MESES)){
    m <- MESES[i]
    pres_m <- pres_data |> filter(mes == m)
    if(nrow(pres_m) == 0 || k_time == 0){
      out_list[[i]] <- NULL
      next
    }
    need_n <- nrow(pres_m) * k_time
    
    # Polígono elegible = jurisdicción menos buffers del mes
    bu <- buffers_union_for_month(pres_m, BUF_M)
    elig_poly <- tryCatch({
      if(!is.null(bu)) sf::st_make_valid(sf::st_difference(juris_union, bu)) else juris_union
    }, error = function(e) juris_union)
    
    if(is.null(elig_poly) || length(elig_poly) == 0){
      out_list[[i]] <- NULL
      next
    }
    
    # Fechas bloqueadas del mes (para ±DT_W)
    blocked <- unique(as.Date(pres_m$Fecha_Inicio))
    blocked <- blocked[!is.na(blocked)]
    
    # Muestra de puntos aleatorios (sobremuestreo para filtrar luego)
    # Nota: st_sample puede devolver menos si el polígono es pequeño -> usamos tries
    tries <- 0; pool <- NULL
    while(tries < 5){
      tries <- tries + 1
      cand <- suppressWarnings(sf::st_sample(elig_poly, size = need_n * 3, type = "random"))
      if(length(cand) == 0) next
      pool <- sf::st_as_sf(data.frame(id = seq_along(cand)), geometry = cand)
      
      # Snap a grilla 100 m
      cc <- sf::st_coordinates(pool)
      pool <- pool |>
        mutate(cell_x = floor(cc[,1]/RES_M)*RES_M,
               cell_y = floor(cc[,2]/RES_M)*RES_M,
               cell_id = paste0(cell_x, "_", cell_y),
               mes = m,
               key = paste(cell_id, mes)) |>
        # descartar celdas con presencia ese mes o duplicadas
        filter(!(key %in% PRES_KEY_SET)) |>
        distinct(cell_id, mes, .keep_all = TRUE)
      
      if(nrow(pool) >= need_n) break
    }
    
    if(is.null(pool) || nrow(pool) == 0){
      out_list[[i]] <- NULL
      next
    }
    
    # Asignar fechas seguras dentro del mes (sin unlist/as.Date(origin=...))
    pool$Fecha <- vapply(seq_len(nrow(pool)), function(ii){
      choose_safe_date(m, blocked, DT_W)
    }, as.Date(Sys.Date()))
    
    pool <- pool |> filter(!is.na(Fecha))
    # coerción defensiva: garantizar clase Date
    pool$Fecha <- as.Date(pool$Fecha)
    
    # Tomar la cantidad necesaria
    pool <- pool |> slice_sample(n = min(need_n, nrow(pool))) |>
      mutate(y = 0L, tipo_ausencia = "time_matched")
    
    # Añadir Direccion y campos auxiliares
    pool <- sf::st_join(pool, juris_proj["Direccion"], left = TRUE)
    
    out_list[[i]] <- pool |>
      mutate(x_coord = cell_x + RES_M/2, y_coord = cell_y + RES_M/2) |>
      # Reemplazar geometría por centro de celda (opcional)
      mutate(geometry = sf::st_as_sfc(mapply(function(cx, cy) sf::st_point(c(cx + RES_M/2, cy + RES_M/2)),
                                             cell_x, cell_y, SIMPLIFY = FALSE), crs = crs_ev)) |>
      select(cell_id, mes, Fecha, y, tipo_ausencia, Direccion, geometry, x_coord, y_coord)
  }
  
  bind_rows(out_list)
}


# =============================================================
# 4) Muestreo SPACE-MATCHED (misma celda, otros meses)
# =============================================================
# Para cada presencia, elige K_SPACE meses (preferencia: mismo mes-del-año en otros años)
# en los que la celda no tenga presencia.

pick_months_space <- function(di, meses_all, blocked_months, k = K_SPACE){
  if(k == 0) return(rep(NA, 0))
  same_moy <- meses_all[month(meses_all) == month(di)]
  cand <- setdiff(same_moy, blocked_months)
  if(length(cand) < k){
    # fallback: cualquier mes no bloqueado
    cand <- unique(c(cand, setdiff(meses_all, blocked_months)))
  }
  if(length(cand) == 0) return(rep(NA, 0))
  sample(cand, size = min(k, length(cand)))
}

# Construir mapa de meses bloqueados por celda (donde hay presencia)
block_by_cell <- pres_cm |>
  st_drop_geometry() |>
  group_by(cell_id) |>
  summarise(blocked = list(unique(mes)), .groups = "drop")

# Generar pseudoausencias space-matched

gen_space_matched <- function(pres_data, k_space = K_SPACE){
  if(k_space == 0) return(NULL)
  pres_tbl <- pres_data |>
    st_drop_geometry() |>
    left_join(block_by_cell, by = "cell_id")
  
  # Centro de celda como geometría estándar
  pres_centers <- pres_data |>
    mutate(geometry = sf::st_as_sfc(mapply(function(cx, cy) sf::st_point(c(cx + RES_M/2, cy + RES_M/2)),
                                           cell_x, cell_y, SIMPLIFY = FALSE), crs = crs_ev)) |>
    select(cell_id, mes, Fecha_Inicio, Direccion, geometry)
  
  res <- pmap_dfr(
    .l = list(
      cell_id = pres_tbl$cell_id,
      mes_i   = pres_tbl$mes,
      di      = pres_tbl$Fecha_Inicio,
      blocked = pres_tbl$blocked
    ),
    .f = function(cell_id, mes_i, di, blocked){
      di <- as.Date(di)
      
      blocked <- blocked[[1]]
      months_pick <- pick_months_space(di, MESES, blocked, k = k_space)
      if(length(months_pick) == 0) return(NULL)
      
      # Fecha candidata: mismo día que el evento (ajustada al mes)
      fechas <- as.Date(vapply(months_pick, function(mo){
        d <- min(day(di), days_in_month(mo))
        as.Date(sprintf("%s-%02d", format(mo, "%Y-%m"), d))
      }, as.Date(Sys.Date())))
      
      # Respetar ±DT_W respecto a di
      keep <- abs(as.numeric(fechas - di)) > DT_W
      months_pick <- months_pick[keep]
      fechas <- fechas[keep]
      if(length(fechas) == 0) return(NULL)
      
      # Construir sf en el centro de la celda
      cx <- as.numeric(strsplit(cell_id, "_")[[1]][1])
      cy <- as.numeric(strsplit(cell_id, "_")[[1]][2])
      geom <- sf::st_as_sfc(lapply(seq_along(fechas), function(k){
        sf::st_point(c(cx + RES_M/2, cy + RES_M/2))
      }), crs = crs_ev)
      
      sf::st_sf(
        cell_id = rep(cell_id, length(fechas)),
        mes     = months_pick,
        Fecha   = fechas,
        y = 0L, tipo_ausencia = "space_matched",
        geometry = geom
      )
    }
  )
  
  if(nrow(res) == 0) return(NULL)
  
  # Quitar choques con presencias existentes
  res <- res |>
    mutate(key = paste(cell_id, mes)) |>
    filter(!(key %in% PRES_KEY_SET)) |>
    select(-key)
  
  # Añadir Direccion
  res <- sf::st_join(res, juris_proj["Direccion"], left = TRUE)
  res
}

# =============================================================
# 5) Ejecutar muestreos y ensamblar salidas
# =============================================================
pa_time  <- gen_time_matched(pres_cm, k_time = K_TIME)
pa_space <- gen_space_matched(pres_cm, k_space = K_SPACE)

pseudo_cm <- bind_rows(pa_time, pa_space) |>
  mutate(origen = "pseudo",
         y = as.integer(y)) |>
  select(cell_id, mes, Fecha, y, tipo_ausencia, origen, geometry) |>
  sf::st_set_geometry("geometry")

pres_out <- pres_cm |>
  mutate(geometry = geom_cell, tipo_ausencia = NA_character_, origen = "presencia", y = 1L) |>
  sf::st_set_geometry("geometry") |>
  select(cell_id, mes, Fecha = Fecha_Inicio, y,
         tipo_ausencia, origen, geometry)


# par(mfrow = c(1,2))
# pres_out %>% select(origen) %>% plot
# pseudo_cm %>% select(origen) %>% plot

# Conjunto final (sin duplicar celda‑mes entre presencias y pseudo)
base_modelo <- bind_rows(pres_out, pseudo_cm) |>
  mutate(key = paste(cell_id, mes)) |>
  distinct(key, .keep_all = TRUE) |>
  select(-key)

#sapply(list(pres_out, pseudo_cm), function(x) names(x))
#st_geometry_type(pres_out)[1]; st_geometry_type(pseudo_cm)[1]
#table(base_modelo$origen, useNA = "ifany")
# base_modelo %>% select(y) %>% plot

# =============================================================
# 6) QA básico: chequear buffers y ventana temporal
# =============================================================
# Distancia mínima a presencias del mismo mes (solo para time-matched)
qa_time <- NULL
if(!is.null(pa_time) && nrow(pa_time) > 0){
  join_m <- inner_join(pa_time |> st_drop_geometry() |> select(cell_id, mes),
                       pres_cm |> st_drop_geometry() |> select(cell_id, mes), by = "mes")
  # Para eficiencia, calculamos por mes
  qa_list <- lapply(unique(pa_time$mes), function(m){
    A <- pa_time |> filter(mes == m)
    B <- pres_cm |> filter(mes == m)
    if(nrow(A) == 0 || nrow(B) == 0) return(NULL)
    dmat <- sf::st_distance(A, B)
    A$dist_min_m <- apply(as.matrix(dmat), 1, min)
    A
  })
  qa_time <- bind_rows(qa_list)
}

# Δ días al evento más cercano del mismo mes (aprox para time-matched)
if(!is.null(qa_time) && nrow(qa_time) > 0){
  # Para cada pseudo, comparar contra fechas de presencias del mes
  qa_time <- qa_time |>
    rowwise() |>
    mutate(delta_dias_min = {
      fechas_pres <- pres_cm |> filter(mes == mes) |> pull(Fecha_Inicio) |> as.Date()
      min(abs(as.numeric(Fecha - fechas_pres)))
    }) |>
    ungroup()
}

# Reinyectar QA al objeto final
if(!is.null(qa_time) && nrow(qa_time) > 0){
  base_modelo <- base_modelo |>
    left_join(qa_time |> st_drop_geometry() |> select(cell_id, mes, dist_min_m, delta_dias_min),
              by = c("cell_id","mes"))
}

# =============================================================
# 7) Resultados
# =============================================================
# # presencias colapsadas (sf)
# presencias_cm <- pres_out
# # pseudoausencias (sf)
# pseudoausencias_cm <- pseudo_cm
# # tabla de modelado (sf: puntos con y, tipo, Direccion, QA)
# # -> usa ‘base_modelo’
# 
# # Rápidos conteos
# cat("Presencias:", nrow(presencias_cm), "
# ")
# cat("Pseudoausencias:", nrow(pseudoausencias_cm), "
# ")
# cat("Base final:", nrow(base_modelo), "
# ")




  
  ## 8) Mapa simple de verificación (y, tipo_ausencia/origen)


# =============================================================
# 8) Mapa simple de verificación
# =============================================================
if (!requireNamespace("tmap", quietly = TRUE)) {
  stop("Instala 'tmap' para el mapa rápido: install.packages('tmap')")
}

# library(tmap)
tmap_mode("plot")

# Copia para estética (factorbinaria y rotulación)
bm_plot <- base_modelo |>
  mutate(
    y_f = factor(y, levels = c(0,1), labels = c("No incendio", "Incendio")),
    tipo_ausencia = ifelse(is.na(tipo_ausencia) & origen == "presencia", "presencia", tipo_ausencia),
    tipo_ausencia = factor(tipo_ausencia, levels = c("presencia", "time_matched", "space_matched"))
  )

# --- QC rápido antes de mapear ---
message("Conteos por origen:")
print(table(bm_plot$origen, useNA = "ifany"))
message("Conteos por etiqueta y:")
print(table(bm_plot$y_f, useNA = "ifany"))

# --- Mapa 1: presencias vs pseudo en capas separadas ---
pres_pts   <- bm_plot |> dplyr::filter(y == 1)
pseudo_pts <- bm_plot |> dplyr::filter(y == 0)

# Si por algún motivo no hay pseudo en base_modelo (p. ej. por 'distinct'),
# usamos directamente 'pseudo_cm' como respaldo visual
if (nrow(pseudo_pts) == 0 && exists("pseudo_cm") && !is.null(pseudo_cm) && nrow(pseudo_cm) > 0) {
  message("[MAPA] No hay pseudo en base_modelo; usando 'pseudo_cm' como respaldo.")
  pseudo_pts <- pseudo_cm
}

map1 <- tm_shape(juris_proj) +
  tm_borders(col = "grey40", lwd = 1) +
  tm_shape(pseudo_pts) +
  tm_dots(size = 0.4, col = "#9E9E9E", alpha = 0.6,
          border.col = "grey20", border.lwd = 0.1) +
  tm_shape(pres_pts) +
  tm_dots(size = 0.4, col = "#E64B35", alpha = 0.6,
          border.col = "black", border.lwd = 0.2) +
  tm_add_legend(type = "symbol",
                labels = c("Pseudoausencia", "Ocurrencia Incendio"),
                col    = c("#9E9E9E", "#E64B35"),
                shape  = 16, size = 0.6, title = "Leyenda:") +
  tm_layout(legend.outside = FALSE,
            legend.position = c("right", "bottom"),
            title = "Ocurrencias y Pseudoausencias") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "arrow", position = c("right", "top"))

# --- Mapa 2: solo pseudoausencias por tipo ---
pseudo_only <- bm_plot |>
  dplyr::filter(origen == "pseudo", !is.na(tipo_ausencia)) |>
  dplyr::mutate(
    tipo_es = dplyr::recode(as.character(tipo_ausencia),
                            "time_matched" = "Mismo mes, diferente lugar",
                            "space_matched" = "Mismo lugar, diferente mes"),
    tipo_es = factor(tipo_es,
                     levels = c("Mismo mes, diferente lugar", "Mismo lugar, diferente mes"))
  ) |>
  dplyr::filter(!is.na(tipo_es))

if (nrow(pseudo_only) == 0 && exists("pseudo_cm") && !is.null(pseudo_cm) && nrow(pseudo_cm) > 0) {
  pseudo_only <- pseudo_cm |>
    dplyr::filter(!is.na(tipo_ausencia)) |>
    dplyr::mutate(
      tipo_es = dplyr::recode(as.character(tipo_ausencia),
                              "time_matched" = "Mismo mes, diferente lugar",
                              "space_matched" = "Mismo lugar, diferente mes"),
      tipo_es = factor(tipo_es,
                       levels = c("Mismo mes, diferente lugar", "Mismo lugar, diferente mes"))
    ) |>
    dplyr::filter(!is.na(tipo_es))
}

map2 <- tm_shape(juris_proj) +
  tm_borders(col = "grey40", lwd = 1) +
  tm_shape(pseudo_only) +
  tm_dots(col = "tipo_es", size = 0.4, style = "cat", alpha = 0.6,
          palette = c("Mismo mes, diferente lugar" = "#4DBBD5",
                      "Mismo lugar, diferente mes" = "darkgreen"),
          legend.show = FALSE,
          border.col = "grey15", border.lwd = 0.2) +
  tm_add_legend(type = "symbol",
                labels = c("Mismo mes, diferente lugar", "Mismo lugar, diferente mes"),
                col    = c("#4DBBD5", "darkgreen"),
                shape  = 16, size = 0.6,
                title  = "Estrategia de pseudoausencia:") +
  tm_layout(legend.outside = FALSE,
            legend.position = c("right", "bottom"), legend.bg.color = "white", legend.frame = TRUE,
            title = "Pseudoausencias por estrategia") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "arrow", position = c("right", "top"))

# Mostrar lado a lado
# print(tmap_arrange(map1, map2, ncol = 2))

