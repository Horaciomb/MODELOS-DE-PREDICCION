###############################
#####IMPLEMENTACION DE PCA#####
###############################

################
####PAQUETES####
################
# Instalar paquetes si no están instalados
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("ggfortify")
#install.packages("caret")

#Apertura
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggfortify)
library(caret)

###########################
####APERTURA Y ANALISIS####
###########################
# Cargar el conjunto de datos 'mtcars'
data(mtcars)

# Estandarizar los datos (importante si las variables están en escalas diferentes)
mtcars_scaled <- scale(mtcars)

#Hagamos un resumen de los datos
summary(mtcars_scaled)


###################
####CORRELACION####
###################
# Matriz de correlación de los datos estandarizados
correlation_matrix <- cor(mtcars_scaled)

# Convertir la matriz en formato largo para ggplot2
melted_correlation <- melt(correlation_matrix)

# Crear el mapa de calor
ggplot(data = melted_correlation, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlación") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title="Mapa de Calor de la Correlación entre Variables")


##########################
####APLICACION DEL PCA####
##########################
# Aplicar PCA
pca_result <- prcomp(mtcars_scaled,
                     center = TRUE,
                     scale. = TRUE)

# Resumen del PCA
summary(pca_result)


########################
####ANALISIS GRAFICO####
########################
# Scree Plot para mostrar la varianza explicada
screeplot(pca_result,
          type = "lines",
          main = "Scree Plot")

# Generar el biplot usando ggplot2
autoplot(pca_result, data = mtcars, 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 3, 
         loadings.label.colour = "blue",
         loadings.colour = "red",
         label.size = 2,        
         label = TRUE) +
  ggtitle("Biplot del PCA") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#########################
####GRAFICO DE PARETO####
#########################
# Varianza explicada por cada componente
var_exp <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Crear un dataframe con los componentes y la varianza explicada
pca_var_exp <- data.frame(Component = seq(1, length(var_exp)), 
                          Variance_Explained = var_exp)

# Añadir una columna de varianza acumulada
pca_var_exp$Cumulative_Variance <- cumsum(pca_var_exp$Variance_Explained)

# Crear el gráfico de Pareto
ggplot(pca_var_exp, aes(x = Component)) +
  geom_bar(aes(y = Variance_Explained),
           stat = "identity", fill = "skyblue") +
  geom_line(aes(y = Cumulative_Variance),
            group = 1, color = "red", size = 1) +
  geom_point(aes(y = Cumulative_Variance),
             color = "red", size = 2) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Varianza Acumulada")) +
  labs(title = "Diagrama de Pareto para el PCA",
       x = "Componentes Principales", 
       y = "Varianza Explicada") +
  theme_minimal()

#############################
####MANEJO DE COMPONENTES####
#############################
# Reducir el conjunto de datos a 2 dimensiones usando los primeros dos componentes principales
pca_data <- as.data.frame(pca_result$x[, 1:2])

# Añadir los nombres de las observaciones (vehículos) para las etiquetas en el gráfico
pca_data$car <- rownames(mtcars)

# Crear un gráfico de dispersión de las observaciones en el espacio reducido (PC1 vs PC2)
ggplot(pca_data, aes(x = PC1, y = PC2, label = car)) +
  geom_point(aes(color = car), size = 3) +  
  ggtitle("Reducción de Dimensionalidad: PCA (PC1 vs PC2)") +
  xlab("Componente Principal 1 (PC1)") +
  ylab("Componente Principal 2 (PC2)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


######################################
####USO EN REGRESIÓN Y COMPARACIÓN####
######################################

#####################################
###PREPARACIÓN DE LA BASE DE DATOS###
#####################################
# Separar la variable dependiente
mpg <- mtcars$mpg

# Crear conjunto de variables predictoras (todas excepto mpg)
predictors <- mtcars[, !names(mtcars) %in% "mpg"]

# Dividir los datos en entrenamiento (70%) y prueba (30%)
set.seed(42)
train_indices <- sample(1:nrow(mtcars), 0.7 * nrow(mtcars))
train_data <- predictors[train_indices, ]
test_data <- predictors[-train_indices, ]
train_mpg <- mpg[train_indices]
test_mpg <- mpg[-train_indices]

######################
###REGRESIÓN LINEAL###
######################
# Modelo con todas las variables predictoras
model_full <- lm(mpg ~ ., data = mtcars)

# Predicción en el conjunto de prueba
pred_test_full <- predict(model_full, newdata = test_data)

# Evaluación con RMSE
rmse_full <- sqrt(mean((test_mpg - pred_test_full)^2))

# Imprimir resultados
print(paste("RMSE Regresión Completa:", round(rmse_full, 2)))

###########
####PCA####
###########
# Estandarizar los datos de las variables predictoras
mtcars_scaled <- scale(predictors)

# Aplicar PCA
pca_result <- prcomp(mtcars_scaled, center = TRUE, scale. = TRUE)

# Ver porcentaje de varianza explicada por cada componente
var_explicada <- summary(pca_result)$importance[2, ]
var_acumulada <- cumsum(var_explicada)

# Seleccionar el número de componentes que explican al menos el 90% de la varianza
num_comp <- which(var_acumulada >= 0.90)[1]

# Imprimir número de componentes seleccionados
print(paste("Número de Componentes Principales seleccionados:", num_comp))

# Visualización del Scree Plot con una línea en el punto de corte del 90% de varianza
screeplot(pca_result, type = "lines", main = "Scree Plot")
abline(v = num_comp, col = "red", lty = 2)

#########################
####REGRESIÓN CON PCA####
#########################
# Extraer los primeros componentes principales seleccionados
pca_train <- as.data.frame(pca_result$x[train_indices, 1:num_comp])
pca_test <- as.data.frame(pca_result$x[-train_indices, 1:num_comp])

# Añadir mpg como variable dependiente
pca_train$mpg <- train_mpg
pca_test$mpg <- test_mpg

# Modelo de regresión con los componentes principales seleccionados
model_pca <- lm(mpg ~ ., data = pca_train)

# Predicción en el conjunto de prueba
pred_test_pca <- predict(model_pca, newdata = pca_test)

# Evaluación con RMSE
rmse_pca <- sqrt(mean((test_mpg - pred_test_pca)^2))

# Imprimir resultados
print(paste("RMSE Regresión con PCA:", round(rmse_pca, 2)))

#################
###COMPARACIÓN###
#################
print(paste("RMSE Regresión Completa:", round(rmse_full, 2)))
print(paste("RMSE Regresión con PCA:", round(rmse_pca, 2)))
