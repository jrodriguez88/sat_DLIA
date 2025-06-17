cob_2024 <- read_sf("data/interm/coberturas/clc_car_ecosistemas_ideam_2024.gpkg")

cob_test <- tibble(cob_2024)


cob_2024


cob_2024$gran_bioma %>% unique()
cob_2024$bioma_prel %>% unique()
cob_2024$ecos_gener %>% unique()
cob_2024$ecos_sinte %>% unique()
cob_2024$paisaje %>% unique()
cob_2024$clima %>% unique()
cob_2024$cob  %>% unique()


gran_bioma
bioma_prel
ecos_sinte
ecos_gener
clima
paisaje
cob




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






