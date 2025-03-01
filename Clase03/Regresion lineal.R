##########################
##### REGRESIÓN LINEAL #####
##########################

################
####PAQUETES####
################
# Instalar paquetes si no están instalados
# install.packages("tidyverse")
# install.packages("car")
# install.packages("caret")

# Apertura de librerías
library(tidyverse)
library(car)
library(caret)


######################
####CARGA DE DATOS####
######################
# Cargar el conjunto de datos mtcars
data(mtcars)

# Dividir los datos en entrenamiento (70%) y prueba (30%)
set.seed(42)
train_indices <- sample(1:nrow(mtcars), 0.7 * nrow(mtcars))
train_data <- mtcars[train_indices, ]
test_data <- mtcars[-train_indices, ]

###############################
####REGRESIÓN LINEAL SIMPLE####
###############################
# Modelo simple
model_simple <- lm(mpg ~ wt, data = train_data)

# Resultados
summary(model_simple)

# Predicción y evaluación
pred_train_simple <- predict(model_simple, newdata = train_data)
pred_test_simple <- predict(model_simple, newdata = test_data)

# Ajuste
rmse_train_simple <- sqrt(mean((train_data$mpg - pred_train_simple)^2))
rmse_test_simple <- sqrt(mean((test_data$mpg - pred_test_simple)^2))

# Visualización
ggplot(train_data, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Regresión Lineal Simple: mpg ~ wt") +
  theme_minimal()

# Imprimir métricas
print(paste("RMSE Entrenamiento (Simple):", round(rmse_train_simple, 2)))
print(paste("RMSE Prueba (Simple):", round(rmse_test_simple, 2)))

#################################
####REGRESIÓN LINEAL MÚLTIPLE####
#################################
# Modelo múltiple
model_multiple <- lm(mpg ~ wt + hp , data = train_data)

# Resultados
summary(model_multiple)

# Evaluación
pred_train_multiple <- predict(model_multiple, newdata = train_data)
pred_test_multiple <- predict(model_multiple, newdata = test_data)

#Ajuste
rmse_train_multiple <- sqrt(mean((train_data$mpg - pred_train_multiple)^2))
rmse_test_multiple <- sqrt(mean((test_data$mpg - pred_test_multiple)^2))

# Diagnóstico de residuos
par(mfrow = c(2, 2))
plot(model_multiple)
par(mfrow = c(1, 1))

# Imprimir métricas
print(paste("RMSE Entrenamiento (Múltiple):", round(rmse_train_multiple, 2)))
print(paste("RMSE Prueba (Múltiple):", round(rmse_test_multiple, 2)))

#####################################
####ANÁLISIS DE MULTICOLINEALIDAD####
#####################################
# Calcular VIF
vif_values <- vif(model_multiple)
print(vif_values)

############################
####MODELOS POLINOMIALES####
############################
# Modelos polinomiales de grados 2 y 3
model_poly2 <- lm(mpg ~ poly(wt, 2), data = train_data)
model_poly3 <- lm(mpg ~ poly(wt, 3), data = train_data)

# Visualización de resultados
summary(model_poly2)
summary(model_poly3)

# Predicciones para grado 2
pred_train_poly2 <- predict(model_poly2, newdata = train_data)
pred_test_poly2 <- predict(model_poly2, newdata = test_data)

# Predicciones para grado 3
pred_train_poly3 <- predict(model_poly3, newdata = train_data)
pred_test_poly3 <- predict(model_poly3, newdata = test_data)

# Evaluación del error para grado 2
rmse_train_poly2 <- sqrt(mean((train_data$mpg - pred_train_poly2)^2))
rmse_test_poly2 <- sqrt(mean((test_data$mpg - pred_test_poly2)^2))

# Evaluación del error para grado 3
rmse_train_poly3 <- sqrt(mean((train_data$mpg - pred_train_poly3)^2))
rmse_test_poly3 <- sqrt(mean((test_data$mpg - pred_test_poly3)^2))

# Visualización grado 2
ggplot(train_data, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +
  ggtitle("Modelo Polinomial (Grado 2)") +
  theme_minimal()

# Visualización grado 3
ggplot(train_data, aes(x = wt, y = mpg)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), color = "green", se = FALSE) +
  ggtitle("Modelo Polinomial (Grado 3)") +
  theme_minimal()

# Imprimir métricas grado 2
print(paste("RMSE Entrenamiento (Polinomial 2):", round(rmse_train_poly2, 2)))
print(paste("RMSE Prueba (Polinomial 2):", round(rmse_test_poly2, 2)))

# Imprimir métricas grado 3
print(paste("RMSE Entrenamiento (Polinomial 3):", round(rmse_train_poly3, 2)))
print(paste("RMSE Prueba (Polinomial 3):", round(rmse_test_poly3, 2)))


################################
####INTERACCIÓN DE VARIABLES####
################################
# Modelo con interacción
model_interaction <- lm(mpg ~ wt * hp, data = train_data)

# Resultados
summary(model_interaction)

# Predicción
pred_train_interaction <- predict(model_interaction, newdata = train_data)
pred_test_interaction <- predict(model_interaction, newdata = test_data)

# Evaluación
rmse_train_interaction <- sqrt(mean((train_data$mpg - pred_train_interaction)^2))
rmse_test_interaction <- sqrt(mean((test_data$mpg - pred_test_interaction)^2))

# Visualización
ggplot(train_data, aes(x = wt, y = mpg, color = hp)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ x * hp, color = "purple", se = FALSE) +
  ggtitle("Modelo con Interacción: mpg ~ wt * hp") +
  theme_minimal()

# Imprimir métricas
print(paste("RMSE Entrenamiento (Interacción):", round(rmse_train_interaction, 2)))
print(paste("RMSE Prueba (Interacción):", round(rmse_test_interaction, 2)))


########################
####PREDICCIÓN FINAL####
########################
# Simulación de un nuevo vehículo con peso 3.5, hp 150, y 6 cilindros
new_data <- data.frame(wt = 3.5, hp = 150, cyl = 6)

# Predicción con cada modelo
pred_lm <- predict(model_multiple, newdata = new_data)
pred_poly <- predict(model_poly2, newdata = new_data)
pred_interaction <- predict(model_interaction, newdata = new_data)

# Mostrar predicciones
print(paste("Predicción Lineal:", round(pred_lm, 2)))
print(paste("Predicción Polinomial:", round(pred_poly, 2)))
print(paste("Predicción Interacción:", round(pred_interaction, 2)))
