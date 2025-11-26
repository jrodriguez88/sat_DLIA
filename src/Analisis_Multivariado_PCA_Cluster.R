# 1) Solo columnas numéricas
fe_num <- to_multivariado %>%
  select(where(is.numeric)) #%>% mutate(Area_Afectada_log = log2(Area_Afectada))

# 2) Quitar a mano cosas que NO quieres en PCA / clustering
#    (IDs, coords, tiempo, códigos de clase, ángulos crudos)
if(isTRUE(base_data)){
  cols_quitar_manualmente <- c(
    "objectid", "latitud", "longitud")
  
} else {
  cols_quitar_manualmente <- c(
    "objectid", "latitud", "longitud",
    "anio", "mes", "dia_mes", "dia_semana",
    "fin_semana", "trimestre", "bimestre", "anio_rel",
    "cobertura", "clc_macro",
    "orientacion", "orient_rad"
  )
  
}



fe_num2 <- fe_num %>%
  select(-any_of(cols_quitar_manualmente))

# 3) Quitar variables discretas tipo dummies / clases
#    (pocas categorías: <= 5 valores distintos)
fe_continuas <- fe_num2 %>%
  select(where(~ dplyr::n_distinct(.x, na.rm = TRUE) > 5))

# 4) Este es tu set para PCA y clustering
vars_pca_cluster <- names(fe_continuas)

fe_pca_cluster <- fe_num %>%
  select(all_of(vars_pca_cluster)) 

# Revisa qué quedó
vars_pca_cluster
str(fe_pca_cluster)



# # library(dplyr)
# # library(dbscan)   # para DBSCAN
# 
# ## 0) Matriz para análisis: filtrar filas completas y escalar ----
# rows_complete <- complete.cases(fe_pca_cluster)
# 
# X          <- fe_pca_cluster[rows_complete, ]
# X_scaled   <- scale(X)   # esta matriz se usa en todo: PCA y clustering
# 
# ## 1) PCA con prcomp ----
# pca_res <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
# 
# # Ejemplos de cosas rápidas a mirar:
# summary(pca_res)          # varianza explicada
# # biplot(pca_res)         # si quieres un biplot básico
# 
# ## 2) k-means ----
# set.seed(123)  # para reproducibilidad
# k_opt <- 3     # ajusta este valor según tu criterio
# 
# kmeans_res <- kmeans(X_scaled, centers = k_opt, nstart = 50)
# 
# ## 3) Clustering jerárquico (hclust) ----
# dist_mat <- dist(X_scaled, method = "euclidean")
# hc_res   <- hclust(dist_mat, method = "ward.D2")
# 
# # Cortar en el mismo número de clusters que k-means (por ejemplo)
# hc_clusters <- cutree(hc_res, k = k_opt)
# 
# ## 4) DBSCAN ----
# # Paso opcional: mirar gráfico kNN para elegir eps
# # kNNdistplot(X_scaled, k = 4)
# # abline(h = 1.5, col = "red", lty = 2)  # ejemplo
# 
# eps_val   <- 3.5   # AJUSTA este valor tras mirar el kNNdistplot
# minPts_val <- 4    # típico: dimensión + 1 o similar
# 
# dbscan_res <- dbscan(X_scaled, eps = eps_val, minPts = minPts_val)
# 
# ## 5) Anexar clusters a la base original ----
# fe_clusters <- test_data %>%
#   mutate(
#     cluster_kmeans = NA_integer_,
#     cluster_hclust = NA_integer_,
#     cluster_dbscan = NA_integer_
#   )
# 
# fe_clusters$cluster_kmeans[rows_complete] <- kmeans_res$cluster
# fe_clusters$cluster_hclust[rows_complete] <- hc_clusters
# fe_clusters$cluster_dbscan[rows_complete] <- dbscan_res$cluster
# 
# ## Objetos clave:
# # - pca_res          : resultados de PCA (cargas, scores, varianza)
# # - kmeans_res       : clustering k-means
# # - hc_res           : árbol jerárquico
# # - dbscan_res       : clustering DBSCAN
# # - fe_clusters      : tabla original + etiquetas de cluster
# 
# 
# 
# 


## 0) Matriz completa y escalada ----
rows_complete <- complete.cases(fe_pca_cluster)

X        <- fe_pca_cluster[rows_complete, ]
X_scaled <- scale(X)



## 1) PCA con prcomp + gráficos factoextra ----
pca_res <- prcomp(X_scaled, center = FALSE, scale. = FALSE)

# Varianza explicada (scree plot)
pca_contrib_comp <- fviz_eig(pca_res, addlabels = TRUE, main = "Varianza explicada por componente")
pca_contrib_var <- factoextra::fviz_contrib(pca_res, choice = "var", , axes = c(1:8), top = 20)

# Variables (cargas y contribución)
pca_biplot_contrib <- fviz_pca_var(
  pca_res,
  col.var = "contrib",      # color según contribución
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  
  title = "Contribucion de las variables en el PCA"
)

# # Individuos en el plano PC1–PC2
# fviz_pca(
#   pca_res,
#   geom.ind = "point",
#   pointshape = 19,
#   pointsize = 2,
#   alpha.ind = 0.7,
#   col.var = "contrib",      # color según contribución
#   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#   repel = TRUE,
#   
#   title = "Individuos en el espacio PCA"
# )

## 2) k-means ----



# 1.1. Elbow / WSS
km_elbow <- fviz_nbclust(
  X_scaled,
  FUNcluster = kmeans,
  method     = "wss",     # Within-cluster sum of squares
  k.max      = 10
) + labs(title = "Numero Optimo de Cluster para Kmeans")

# 1.2. Silhouette promedio
km_silueta <- fviz_nbclust(
  X_scaled,
  FUNcluster = kmeans,
  method     = "silhouette",
  k.max      = 10
) + labs(title = "")

# 1.3. Gap Statistic
set.seed(123)
gap_res <- clusGap(X_scaled, FUN = kmeans, K.max = 10, B = 50)
km_gap <- fviz_gap_stat(gap_res) + labs(title = "")

plot_Ncluster_km <- km_elbow | km_silueta | km_gap

## Definir Cluster Kmeans ----
k_opt      <- 2                   # ajusta según tu criterio
kmeans_res <- kmeans(X_scaled, centers = k_opt, nstart = 50)

# PCA coloreado por k-means
pca_kmeans <- fviz_pca_ind(
  pca_res,
  geom.ind  = "point",
  habillage = factor(kmeans_res$cluster),
  addEllipses = TRUE,
  ellipse.level = 0.95,
  title = "PCA – individuos coloreados por k-means"
)

# Vista clásica de cluster sobre el espacio PCA (factoextra hace la proyección)
# fviz_cluster(
#   kmeans_res,
#   data = X_scaled,
#   geom = "point",
#   ellipse.type = "norm",
#   main = "k-means (k = 2) sobre variables estandarizadas"
# )


## 3) Clustering jerárquico (hclust) + plot en PCA ----
# 2.1. WSS para jerárquico
hc_elbow <- fviz_nbclust(
  X_scaled,
  FUNcluster = hcut,      # jerárquico
  method     = "wss",
  k.max      = 10
) + labs(title = "Numero Optimo de Cluster para Cluster Jerarquico")

# 2.2. Silhouette para jerárquico
hc_silueta <- fviz_nbclust(
  X_scaled,
  FUNcluster = hcut,
  method     = "silhouette",
  k.max      = 10
) + labs(title = "")

# 1.3. Gap Statistic
set.seed(123)
gap_res_hcl <- clusGap(X_scaled, FUN = hcut, K.max = 10, B = 50)
hc_gap <- fviz_gap_stat(gap_res_hcl) + labs(title = "")

plot_Ncluster_hc <- hc_elbow | hc_silueta | hc_gap

k_opt <- 2


dist_mat   <- dist(X_scaled, method = "euclidean")
hc_res     <- hclust(dist_mat, method = "ward.D2")
hc_clusters <- cutree(hc_res, k = k_opt)

# Dendrograma
plot(hc_res, labels = FALSE, main = "Clustering jerárquico (Ward.D2)")
rect.hclust(hc_res, k = k_opt, border = "red")

# Don't color labels, add rectangles
fviz_dend(hc_res, cex = 0.5, k = 2,
          color_labels_by_k = FALSE, rect = TRUE)

# PCA coloreado por hclust
pca_hclust <- fviz_pca_ind(
  pca_res,
  geom.ind  = "point",
  habillage = factor(hc_clusters),
  addEllipses = TRUE,
  ellipse.level = 0.95,
  title = "PCA – individuos coloreados por hclust"
)

# Vista tipo fviz_cluster con hclust
# fviz_cluster(
#   list(data = X_scaled, cluster = hc_clusters),
#   geom = "point", 
#   ellipse.type = "norm",
#   main = "Clusters hclust (Ward.D2)"
# )

## 4) DBSCAN + visualización en PCA ----
# (idealmente elegir eps con kNNdistplot antes)
#kNNdistplot(X_scaled, k = 10); abline( = 1.5, col = "red", lty = 2)

kNNdistplot(X_scaled, k = 1:20)  ; abline(h = 5, col = "red", lty = 2)


eps_val    <- 5   # AJUSTA
minPts_val <- 8     # típico ~ dimensión + 1

dbscan_res <- dbscan(X_scaled, eps = eps_val, minPts = minPts_val)
table(dbscan_res$cluster) 

# Etiquetas de DBSCAN (0 = ruido)
db_labels <- ifelse(dbscan_res$cluster == 0,
                    "ruido",
                    paste0("C", dbscan_res$cluster))
# db_labels <- factor(db_labels)

# PCA coloreado por DBSCAN
pca_dbscan <- fviz_pca_ind(
  pca_res,
  geom.ind  = "point",
  habillage = factor(db_labels),
  addEllipses = FALSE,
  title = "PCA – clusters DBSCAN (ruido vs grupos)"
)

# labels_db <- ifelse(dbscan_res$cluster == 0, "ruido", paste0("C", dbscan_res$cluster))
# 
# fviz_pca_ind(
#   pca_res,
#   habillage = factor(labels_db),
#   geom.ind  = "point",
#   addEllipses = TRUE,
#   title = "PCA + DBSCAN"
# )



## 5) Opcional: pegar clusters a la tabla original ----
fe_clusters <- to_multivariado %>%
  mutate(
    cluster_kmeans = NA_integer_,
    cluster_hclust = NA_integer_,
    cluster_dbscan = NA_character_
  )

fe_clusters$cluster_kmeans[rows_complete] <- kmeans_res$cluster
fe_clusters$cluster_hclust[rows_complete] <- hc_clusters
fe_clusters$cluster_dbscan[rows_complete] <- as.character(db_labels)


# 
map_kmeans <- ggplot() +
  geom_sf(data = juris_proj, fill = NA, color = "black") +
  geom_point(data = fe_clusters,
             aes(x = longitud, y = latitud, color = factor(cluster_kmeans)),
             size = 1.5) +
  theme_minimal()

map_hclust <- ggplot() +
  geom_sf(data = juris_proj, fill = NA, color = "black") +
  geom_point(data = fe_clusters,
             aes(x = longitud, y = latitud, color = factor(cluster_hclust)),
             size = 1.5) +
  theme_minimal()

map_dbscan <- ggplot() +
  geom_sf(data = juris_proj, fill = NA, color = "black") +
  geom_point(data = fe_clusters,
             aes(x = longitud, y = latitud, color = factor(cluster_dbscan)),
             size = 1.5) +
  theme_minimal()
# 

# ggplot() +
#   geom_spatraster(data = elevacion_car) +
#   scale_fill_viridis_c(option = "terrain", name = "Elevación (m)") +
#   geom_sf(data = juris_proj, fill = NA, color = "black", size = 0.7) +
#   geom_point(data = fe_clusters,
#              aes(x = longitud, y = latitud, color = factor(cluster_hclust)),
#              size = 1.5) +
#   theme_minimal()


# Analisis PCA
(pca_biplot_contrib | pca_contrib_comp  / pca_contrib_var )

print(plot_Ncluster_km)
print(plot_Ncluster_hc)
kNNdistplot(X_scaled, k = 1:20)  ; abline(h = 5, col = "red", lty = 2)


(pca_kmeans | pca_hclust | pca_dbscan) 




par(mfrow = c(1, 3))

# Raster al fondo
plot(elevacion_car, col = terrain.colors(50),
     main = "Clusters Kmeans")
# Polígono encima, sin relleno (solo borde negro)
plot(juris_proj, add = TRUE, border = "black", col = NA, lwd = 1)
# Colores para los clusters
cols <- rainbow(length(unique(fe_clusters$cluster_kmeans)))
# Puntos encima
points(fe_clusters$longitud,
       fe_clusters$latitud,
       col = cols[fe_clusters$cluster_kmeans],
       pch = 19, cex = 1)
# Leyenda opcional
legend("topright", legend = unique(fe_clusters$cluster_kmeans),
       col = cols, pch = 19, title = "Cluster")


# Raster al fondo
plot(elevacion_car, col = terrain.colors(50),
     main = "Clusters Jerarquico")
# Polígono encima, sin relleno (solo borde negro)
plot(juris_proj, add = TRUE, border = "black", col = NA, lwd = 1)
# Colores para los clusters
cols <- rainbow(length(unique(fe_clusters$cluster_hclust)))
# Puntos encima
points(fe_clusters$longitud,
       fe_clusters$latitud,
       col = cols[fe_clusters$cluster_hclust],
       pch = 19, cex = 1)
# Leyenda opcional
legend("topright", legend = unique(fe_clusters$cluster_hclust),
       col = cols, pch = 19, title = "Cluster")


# Raster al fondo
plot(elevacion_car, col = terrain.colors(50),
     main = "Clusters DBSCAN")
# Polígono encima, sin relleno (solo borde negro)
plot(juris_proj, add = TRUE, border = "black", col = NA, lwd = 1)
# Colores para los clusters
cols <- rainbow(length(unique(fe_clusters$cluster_dbscan)))
# Puntos encima
points(fe_clusters$longitud,
       fe_clusters$latitud,
       col = cols[as.numeric(factor(fe_clusters$cluster_dbscan))],
       pch = 19, cex = 1)
# Leyenda opcional
legend("topright", legend = unique(fe_clusters$cluster_dbscan),
       col = cols, pch = 19, title = "Cluster")



