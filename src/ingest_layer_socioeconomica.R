

# Source path 
raw_data_se <- "data/raw/socioeconomicos/tabla_maestro_municipios_CAR.xlsx"


dir.create(dir_socioeconomic)

raw_se <- read_excel(raw_data_se, sheet = "in", skip = 1) %>% 
  dplyr::select(-c(Municipio:Departamen)) %>% 
  mutate(CODDANE = as.character(CODDANE))

socioeconomic_data_spatial <- municipios %>% left_join(raw_se)


sf::st_write(
  obj      = socioeconomic_data_spatial,
  dsn      = file.path(dir_socioeconomic, paste0("socioeconomic_data_spatial", ".gpkg")),
  delete_dsn = TRUE,    # sobrescribe si ya existía
  quiet    = TRUE
)


# vect("data/interm/socioeconomica/test.gpkg")

# plot(socioeconomic_data_spatial[8:13])
# 
# plot(socioeconomic_data_spatial[8])
