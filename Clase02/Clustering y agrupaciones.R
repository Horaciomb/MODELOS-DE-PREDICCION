###################################
#####CLUSTERING Y AGRUPACIONES#####
###################################

################
####PAQUETES####
################
# Instalar paquetes si no están instalados
#install.packages("ggplot2")
#install.packages("cluster")
#install.packages("dbscan")
#install.packages("isotree")

#Apertura
library(ggplot2)
library(cluster)
library(dbscan)
library(isotree)

################
####APERTURA####
################
data("mtcars")

###############################
#####METODOLOGIAS CLASICAS#####
###############################

###########
####IQR####
###########
# Calcular el IQR de 'mpg'
Q1 <- quantile(mtcars$mpg, 0.25)
Q3 <- quantile(mtcars$mpg, 0.75)
IQR_valor <- IQR(mtcars$mpg)

# Definir límites
inferior <- Q1 - 1.5 * IQR_valor
superior <- Q3 + 1.5 * IQR_valor

# Identificar outliers
outliers_iqr <- which(mtcars$mpg < inferior | mtcars$mpg > superior)

# Visualización
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue", size = 3) +
  geom_point(data = mtcars[outliers_iqr, ], aes(x = wt, y = mpg),
             color = "red", size = 4) +
  ggtitle("Outliers en mpg detectados con IQR") +
  xlab("Peso (wt)") + ylab("Millas por galón (mpg)") +
  theme_minimal()


###############
####Z-Score####
###############
# Calcular el Z-Score para 'mpg'
z_scores <- scale(mtcars$mpg)
outliers_zscore <- which(abs(z_scores) > 3)

# Visualización
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue", size = 3) +
  geom_point(data = mtcars[outliers_zscore, ], aes(x = wt, y = mpg),
             color = "red", size = 4) +
  ggtitle("Outliers en mpg detectados con Z-Score") +
  xlab("Peso (wt)") + ylab("Millas por galón (mpg)") +
  theme_minimal()


################################
####Distancia de Mahalanobis####
################################
# Seleccionar variables para análisis
mtcars_data <- mtcars[, c("mpg", "wt", "hp")]

# Calcular la distancia de Mahalanobis
mahal_dist <- mahalanobis(mtcars_data, colMeans(mtcars_data), cov(mtcars_data))

# Definir umbral usando la distribución chi-cuadrado (p < 0.01)
p_value_cutoff <- 0.01
chi_square_cutoff <- qchisq(1 - p_value_cutoff, df = ncol(mtcars_data))

# Identificar outliers
outliers_mahal <- which(mahal_dist > chi_square_cutoff)

# Visualización
plot(mtcars$wt, mtcars$mpg,
     main = "Outliers detectados con Distancia de Mahalanobis",
     xlab = "Peso (wt)", ylab = "Millas por galón (mpg)",
     pch = 16, col = "blue")
points(mtcars$wt[outliers_mahal], mtcars$mpg[outliers_mahal],
       col = "red", pch = 19, cex = 1.5)


#####################################
#####METODOLOGIAS CON CLUSTERING#####
#####################################

###############
####K-Means####
###############
# Escalar datos
mtcars_scaled <- scale(mtcars)

# Aplicar K-Means con 3 clusters
set.seed(42)
kmeans_resultado <- kmeans(mtcars_scaled, centers = 3)

# Calcular la distancia de cada punto al centroide de su cluster
centros <- kmeans_resultado$centers
centros_asignados <- centros[kmeans_resultado$cluster, ]
distancias <- sqrt(rowSums((mtcars_scaled - centros_asignados)^2))

# Definir umbral de outliers (por ejemplo, percentil 95 de las distancias)
threshold <- quantile(distancias, 0.95)
outliers_kmeans <- which(distancias > threshold)

# Visualización
mtcars$cluster <- as.factor(kmeans_resultado$cluster)
ggplot(mtcars, aes(x = wt, y = mpg, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = mtcars[outliers_kmeans, ], aes(x = wt, y = mpg),
             color = "red", size = 4) +
  ggtitle("Detección de Outliers con K-Means") +
  xlab("Peso (wt)") + ylab("Millas por galón (mpg)") +
  theme_minimal()


#############################
####Clustering jerarquico####
#############################
# Clustering jerárquico
hclust_result <- hclust(dist(mtcars_scaled), method = "complete")

# Visualización del dendrograma
plot(hclust_result,
     labels = rownames(mtcars),
     main = "Dendrograma del Clustering Jerárquico")

# Cortar el dendrograma en 3 clusters
clusters_hier <- cutree(hclust_result, k = 3)
mtcars$cluster_hier <- as.factor(clusters_hier)

# Suponiendo que un cluster con muy pocos elementos (por ejemplo, < 3) sea considerado outlier
table(clusters_hier)
small_clusters <- names(which(table(clusters_hier) < 3))
outliers_hier <- which(as.character(clusters_hier) %in% small_clusters)

# Visualización: Scatterplot con outliers identificados del clustering jerárquico
ggplot(mtcars, aes(x = wt, y = mpg, color = cluster_hier)) +
  geom_point(size = 3) +
  geom_point(data = mtcars[outliers_hier, ], aes(x = wt, y = mpg),
             color = "red", size = 4, shape = 8) +
  ggtitle("Detección de Outliers con Clustering Jerárquico") +
  xlab("Peso (wt)") + ylab("Millas por galón (mpg)") +
  theme_minimal()


##############
####DBSCAN####
##############
# Aplicar DBSCAN (valores aproximados de eps y minPts)
dbscan_result <- dbscan(mtcars_scaled, eps = 2.5, minPts = 3)

# Identificar outliers (puntos con cluster == 0)
outliers_dbscan <- which(dbscan_result$cluster == 0)

# Visualización
mtcars$cluster_dbscan <- as.factor(dbscan_result$cluster)
ggplot(mtcars, aes(x = wt, y = mpg, color = cluster_dbscan)) +
  geom_point(size = 3) +
  geom_point(data = mtcars[outliers_dbscan, ], aes(x = wt, y = mpg),
             color = "red", size = 4) +
  ggtitle("Detección de Outliers con DBSCAN") +
  xlab("Peso (wt)") + ylab("Millas por galón (mpg)") +
  theme_minimal()


########################
####Isolation Forest####
########################
# Entrenar el modelo de Isolation Forest con variables mpg, wt y hp
iso_model <- isolation.forest(mtcars_data, ndim = 1)

# Predecir puntajes de anomalía
iso_scores <- predict(iso_model, mtcars_data)

# Definir umbral y detectar outliers
outliers_iso <- which(iso_scores > 0.65)

# Visualización
plot(mtcars$wt, mtcars$mpg,
     main = "Outliers detectados con Isolation Forest",
     xlab = "Peso (wt)", ylab = "Millas por galón (mpg)",
     pch = 16, col = "blue")
points(mtcars$wt[outliers_iso], mtcars$mpg[outliers_iso],
       col = "red", pch = 19, cex = 1.5)


##################################
####Local Outlier Factor (LOF)####
##################################
# Calcular LOF para las variables seleccionadas
lof_scores <- lof(mtcars_data, minPts = 6)

# Identificar outliers
outliers_lof <- which(lof_scores > 1.5)

# Visualización
plot(mtcars$wt, mtcars$mpg,
     main = "Outliers detectados con LOF",
     xlab = "Peso (wt)", ylab = "Millas por galón (mpg)",
     pch = 16, col = "blue")
points(mtcars$wt[outliers_lof], mtcars$mpg[outliers_lof],
       col = "red", pch = 19, cex = 1.5)
