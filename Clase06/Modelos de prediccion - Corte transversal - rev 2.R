###################################
#####PREDICCION PRECIO DE CASA#####
###################################

#################
####LIBRERIAS####
#################
library(tidyverse)
library(ggplot2)
library(cluster)
library(fastDummies)
library(dbscan)
library(caret)
library(glmnet)

################
####APERTURA####
################
#Abrimos la base de datos
data <- read.csv("D:/Diplomado UPB/Prediccion Analitica/Bases de datos/Bases de datos/data.csv")


#####################################
####ANALISIS GENERAL DE LOS DATOS####
#####################################
#Analisis general
summary(data)

#Analisis mas detallado
sapply(data,
       function(x) if(is.numeric(x)) c(Media = mean(x, na.rm = TRUE),
                                             Desviación = sd(x, na.rm = TRUE),
                                             Mínimo = min(x, na.rm = TRUE),
                                             Máximo = max(x, na.rm = TRUE)))

#Analisis de variables en texto
chr_vars <- data[sapply(data, is.character)]

#Veamos las variables de texto
head(chr_vars)

#Nos quedamos solo con aquellas que son categoricas
chr_vars <- chr_vars[, c('city', 'country')]

# Convertir todas las variables de texto (character) en factores
data$city <- as.factor(data$city)
data$country <- as.factor(data$country)

#Analisis de frecuencia
table(data$city)
table(data$country)


###########################################
####ANALISIS DE LA VARIABLE DEPENDIENTE####
###########################################
#Histograma de precio
ggplot(data, aes(x = price)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histograma de precio") +
  xlab("Precio") + ylab("Frecuencia") +
  theme_minimal()

#Posible transformaciones
data$price_ln <- log(data$price)

#Histograma del logaritmo del precio
ggplot(data, aes(x = price_ln)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histograma del logaritmo precio") +
  xlab("Precio en logaritmo") + ylab("Frecuencia") +
  theme_minimal()

#Z-Score
data$price_z <- scale(data$price)

#Histograma del precio normalizado
ggplot(data, aes(x = price_z)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histograma del precio normalizado") +
  xlab("Precio normalizado") + ylab("Frecuencia") +
  theme_minimal()


########################
####LIMPIEZA DE BASE####
########################
###Tratamiento solo variables numericas
#Elimamos las variables tipo texto que no nos sirven
data <- data %>%
  select(-c(date, street, statezip,
            price_ln, price_z, country))

# Crear variables dummy y añadirlas al dataframe original
data_final <- dummy_cols(data, 
                         remove_first_dummy = FALSE,
                         remove_selected_columns = TRUE)

# Hacemos una lista con las dummies
dummy_columns_city <- grep("city_", names(data_final), value = TRUE)

# Seleccionamos todos los nombres
nombres_generales <- names(data_final)

# Hacemos una diferencia de esas listas
resto_columnas <- setdiff(nombres_generales, dummy_columns_city)

# Escalamos solo aquellas que no son dummies
data_scaled <- scale(data_final[, resto_columnas])

#Unamos las 2 bases
data_scaled <- cbind(data_scaled, data_final[, dummy_columns_city])

###Valores nulos
# Contar los valores NA por cada columna
na_por_columna <- apply(data_scaled, 2, function(x) sum(is.na(x)))

# Mostrar el número de NA por columna
na_por_columna

###Valores nulos
# Contar los valores NAN por cada columna
nan_por_columna <- apply(data_scaled, 2, function(x) sum(is.nan(x)))

# Mostrar el número de NA por columna
nan_por_columna

###Valores infinitos
# Contar los valores infinitos por cada columna
inf_por_columna <- apply(data_scaled, 2, function(x) sum(is.infinite(x)))

# Mostrar el número de NA por columna
inf_por_columna

# Eliminar filas que contengan cualquier valor Inf o -Inf
data_clean <- data_scaled[apply(data_scaled, 1, function(x) all(!is.infinite(x))), ]

###Valores infinitos
# Contar los valores infinitos por cada columna
inf_por_columna <- apply(data_scaled, 2, function(x) sum(is.infinite(x)))

# Mostrar el número de NA por columna
inf_por_columna


###########################################
####ANALISIS DE OUTLIERS USANDO K-MEANS####
###########################################
# Aplicar K-Means con un rango de clusters
set.seed(42)

# Rango de valores de k
k_values <- 2:10

# Crear una lista para almacenar los valores del Índice de Silueta
silhouette_scores <- numeric(length(k_values))

# Iterar sobre los diferentes valores de k
for (i in 1:length(k_values)) {
  kmeans_result <- kmeans(data_clean,
                          centers = k_values[i])
  silhouette_result <- silhouette(kmeans_result$cluster,
                                  dist(data_scaled))
  silhouette_scores[i] <- mean(silhouette_result[, 3])
}

# Mostrar los resultados de los índices de silueta para cada k
silhouette_scores

# Encontrar el valor óptimo de k (el que maximiza el Índice de Silueta)
optimal_k <- k_values[which.max(silhouette_scores)]
print(paste("El número óptimo de clusters es:", optimal_k))

# Crear un gráfico del Índice de Silueta vs. k
plot(k_values, silhouette_scores, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters K",
     ylab = "Índice de Silueta Promedio",
     main = "Índice de Silueta para cada K")

# Aplicamos K-Means para detectar outliers
kmeans_result <- kmeans(data_clean, centers = optimal_k)

data_clean$cluster <- as.factor(kmeans_result$cluster)

# Calculamos la distancia de cada punto al centroide de su cluster
centroids <- kmeans_result$centers
distances <- apply(data_scaled, 1, function(row) {
  row <- as.numeric(row)  # Asegurar que 'row' es un vector numérico
  cluster <- kmeans_result$cluster[which.min(rowSums((centroids - row)^2))]
  sqrt(sum((row - centroids[cluster, ])^2))
})
data_clean$distance <- distances

# Definimos un umbral para detectar outliers (ej. percentil 95)
threshold <- quantile(distances, 0.95)
outliers <- data_clean %>% filter(distance > threshold)

# Eliminamos los outliers de los datos
filtered_data <- data_clean %>% filter(distance <= threshold)

# Ver el número de outliers detectados
print(paste("Número de outliers detectados:", nrow(outliers)))

# Eliminamos la variable de cluster, distancia y waterfront
filtered_data <- filtered_data %>%
  select(-c(cluster, distance, waterfront))

# Eliminamos a las casas con valor de 0
filtered_data <- filtered_data %>%
  filter(price != 0)

########################################
####PARTICIÓN PRUEBA Y ENTRENAMIENTO####
########################################
set.seed(42)
train_index <- createDataPartition(filtered_data$price,
                                   p = 0.8,
                                   list = FALSE)

# Crear conjunto de entrenamiento y prueba
train_data <- filtered_data[train_index, ]
test_data <- filtered_data[-train_index, ]

# Verificar las dimensiones de los conjuntos
dim(train_data)
dim(test_data)


#################################
####REGRESION LINEAL SENCILLA####
#################################
#Eliminamos una dummy de cada categoría para evitar multicolinealidad perfecta
data_lm <- train_data %>%
  select(-c(city_Algona))
test_data_lm <- test_data %>%
  select(-c(city_Algona))

#Hacemos una regresion con todas las variables
lm_model <- lm(price ~ ., data = data_lm)

#Vemos los resultados
summary(lm_model)

# Diagnóstico de los residuos
par(mfrow = c(2, 2))
plot(lm_model)
par(mfrow = c(1, 1))

# Evaluación
pred_test_lm <- predict(lm_model, newdata = test_data_lm)

#Ajuste
rmse_test_lm <- sqrt(mean((test_data$price - pred_test_lm)^2))

# Imprimir métricas
print(paste("RMSE Solo regresión:", round(rmse_test_lm, 3)))

# Crear un dataframe con los valores reales y predichos
results <- data.frame(Actuales = test_data_lm$price,
                      Predictions = pred_test_lm)

# Crear el scatterplot entre las predicciones y los valores reales
ggplot(results, aes(x = Actuales, y = Predictions)) +
  geom_point(color = "blue", alpha = 0.6) + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatterplot de Predicciones vs Actuales",
       x = "Valores Reales (Actuales)",
       y = "Valores Predichos (Predicciones)") +
  theme_minimal()


##############################
####REGRESION LINEAL + PCA####
##############################
#Eliminamos precio
train_data_no_price <- train_data %>%
  select( -c("price"))

# Aplicar PCA al conjunto de entrenamiento
pca_result <- prcomp(train_data_no_price,
                     center = TRUE,
                     scale. = TRUE)

# Ver los resultados de PCA
summary(pca_result)

# Graficar la varianza explicada por los componentes principales
plot(pca_result, type = "l", main = "Scree Plot")

# Seleccionar los primeros componentes principales que expliquen, por ejemplo, el 95% de la varianza
var_explicada <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
num_componentes <- which(var_explicada >= 0.90)[1]

# Crear un nuevo conjunto de entrenamiento basado en los componentes principales
train_pca <- pca_result$x[, 1:num_componentes]

# Lo ponemos un formato de dataframe
train_pca <- as.data.frame(train_pca)

# Añadir el valor de 'mpg' como variable dependiente
train_pca$price <- train_data$price

# Ajustar el modelo de regresión lineal con los componentes principales
lm_pca <- lm(price ~ . , data = train_pca)

# Ver el resumen del modelo
summary(lm_pca)

# Proyectar el conjunto de prueba en los mismos componentes principales del entrenamiento
test_pca <- predict(pca_result,
                    newdata = test_data)[, 1:num_componentes]

# Predecir los valores de 'price' usando el modelo ajustado en el conjunto de prueba
predictions <- predict(lm_pca,
                       newdata = data.frame(test_pca))

# Evaluar el desempeño del modelo en el conjunto de prueba
actuals <- test_data$price
mse <- mean((predictions - actuals)^2)
rmse <- sqrt(mse)

# Mostrar el desempeño
print(paste("RMSE Regresión + PCA:", round(rmse, 3)))

# Crear un dataframe con los valores reales y predichos
results <- data.frame(Actuales = actuals,
                      Predictions = predictions)

# Crear el scatterplot entre las predicciones y los valores reales
ggplot(results, aes(x = Actuales, y = Predictions)) +
  geom_point(color = "blue", alpha = 0.6) + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatterplot de Predicciones vs Actuales",
       x = "Valores Reales (Actuales)",
       y = "Valores Predichos (Predicciones)") +
  theme_minimal()


######################################################
####REGRESION LINEAL SENCILLA DE DIFERENTES GRADOS####
######################################################
#Añadimos elementos de diferentes grados en entrenamiento
data_lm <- data_lm %>%
  mutate(sqft_living2 = sqft_living^2,
         sqft_living3 = sqft_living^3,
         sqft_lot2 = sqft_lot^2,
         sqft_lot3 = sqft_lot^3)

#Hacemos una regresion con todas las variables
lm_model_pol <- lm(price ~ ., data = data_lm)

#Vemos los resultados
summary(lm_model_pol)

# Diagnóstico de los residuos
par(mfrow = c(2, 2))
plot(lm_model_pol)
par(mfrow = c(1, 1))

#Añadimos los elementos de diferentes grados en prueba
test_data_lm <- test_data_lm %>%
  mutate(sqft_living2 = sqft_living^2,
         sqft_living3 = sqft_living^3,
         sqft_lot2 = sqft_lot^2,
         sqft_lot3 = sqft_lot^3)

# Evaluación
pred_test_lm <- predict(lm_model_pol, newdata = test_data_lm)

#Ajuste
rmse_test_lm <- sqrt(mean((test_data_lm$price - pred_test_lm)^2))

# Imprimir métricas
print(paste("RMSE Solo regresión polinómica:", round(rmse_test_lm, 3)))

# Crear un dataframe con los valores reales y predichos
results <- data.frame(Actuales = test_data_lm$price,
                      Predictions = pred_test_lm)

# Crear el scatterplot entre las predicciones y los valores reales
ggplot(results, aes(x = Actuales, y = Predictions)) +
  geom_point(color = "blue", alpha = 0.6) + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatterplot de Predicciones vs Actuales",
       x = "Valores Reales (Actuales)",
       y = "Valores Predichos (Predicciones)") +
  theme_minimal()


####################################
####REGRESIONES DE RIDGE Y LASSO####
####################################
# Definimos matrices de predictores (X) y variable dependiente (Y)
X_train <- as.matrix(data_lm %>% select(-price))
Y_train <- data_lm$price

X_test <- as.matrix(test_data_lm %>% select(-price))
Y_test <- test_data_lm$price

# Definimos grid de lambdas
lambda_grid <- 10^seq(4, -2, length = 100)

### RIDGE REGRESSION (alpha = 0)
ridge_cv <- cv.glmnet(X_train, Y_train, alpha = 0, lambda = lambda_grid)
best_lambda_ridge <- ridge_cv$lambda.min

ridge_model <- glmnet(X_train, Y_train, alpha = 0, lambda = best_lambda_ridge)
ridge_preds <- predict(ridge_model, newx = X_test)

ridge_rmse <- RMSE(ridge_preds, Y_test)

### LASSO REGRESSION (alpha = 1)
lasso_cv <- cv.glmnet(X_train, Y_train, alpha = 1, lambda = lambda_grid)
best_lambda_lasso <- lasso_cv$lambda.min

lasso_model <- glmnet(X_train, Y_train, alpha = 1, lambda = best_lambda_lasso)
lasso_preds <- predict(lasso_model, newx = X_test)

lasso_rmse <- RMSE(lasso_preds, Y_test)

# Mostrar coeficientes de la regresión Ridge
cat("Coeficientes de Ridge:\n")
print(coef(ridge_model))

# Mostrar coeficientes de la regresión Lasso
cat("\nCoeficientes de Lasso:\n")
print(coef(lasso_model))

# Mostramos los RMSE
cat("RMSE Ridge:", ridge_rmse, "\n")
cat("RMSE Lasso:", lasso_rmse, "\n")

# Convertir predicciones a vectores
ridge_preds <- as.vector(ridge_preds)
lasso_preds <- as.vector(lasso_preds)

# Graficamos predicciones vs valores reales
ridge_plot <- ggplot(data.frame(Real = Y_test, Pred = ridge_preds), aes(x = Real, y = Pred)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Ridge: Predicciones vs Real", x = "Precio Real", y = "Predicción") +
  theme_minimal()

lasso_plot <- ggplot(data.frame(Real = Y_test, Pred = lasso_preds), aes(x = Real, y = Pred)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Lasso: Predicciones vs Real", x = "Precio Real", y = "Predicción") +
  theme_minimal()

# Mostrar gráficos
print(ridge_plot)
print(lasso_plot)