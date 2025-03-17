###############################
##### REGRESIÓN LOGÍSTICA #####
###############################

################
####PAQUETES####
################
# Instalar paquetes si no están instalados
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("GGally")

# Cargar librerías
library(tidyverse)
library(caret)
library(GGally)

######################
####CARGA DE DATOS####
######################
# Cargar el dataset
data <- read.csv("D:/Diplomado UPB/Prediccion Analitica/Bases de datos/Bases de datos/Fertility_Dataset.csv")


# Mostrar estructura de datos
glimpse(data)

#################################
#### PREPROCESAMIENTO DE DATOS ###
#################################
# Convertir Diagnosis a binaria (1 = Altered, 0 = Normal)
data$Diagnosis <- ifelse(data$Diagnosis == "Altered", 1, 0)

# Convertir variables categóricas en dummies
data_matrix <- model.matrix(Diagnosis ~ . - 1, data = data) %>% as.data.frame()

# Volver a agregar Diagnosis al dataset
data_matrix$Diagnosis <- as.factor(data$Diagnosis) 

# Verificar estructura
glimpse(data_matrix)

###############################
####DIVISIÓN TRAIN / TEST####
###############################
set.seed(42)  # Reproducibilidad
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

######################################
#### AJUSTE DEL MODELO LOGÍSTICO ####
######################################
logit_model <- glm(Diagnosis ~ ., data = train_data, family = binomial)

# Resumen del modelo
summary(logit_model)

#################################
#### EVALUACIÓN DEL MODELO ####
#################################
# Predicciones en test
test_data$pred_prob <- predict(logit_model, newdata = test_data, type = "response")
test_data$pred_class <- ifelse(test_data$pred_prob > 0.3, 1, 0)

# Matriz de Confusión
conf_matrix <- table(Real = test_data$Diagnosis, Predicho = test_data$pred_class)
print(conf_matrix)

# Métricas de evaluación
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2,2] / sum(conf_matrix[,2])
recall <- conf_matrix[2,2] / sum(conf_matrix[2,])

# Mostrar métricas
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")

###############################
#### CURVA ROC Y AUC ####
###############################
library(pROC)
roc_curve <- roc(test_data$Diagnosis, test_data$pred_prob)
plot(roc_curve, col = "blue", main = "Curva ROC")
auc(roc_curve)