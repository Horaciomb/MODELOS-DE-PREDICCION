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
# install.packages("pROC")

# Cargar librerías
library(tidyverse)
library(caret)
library(GGally)
library(pROC)

######################
####CARGA DE DATOS####
######################
# Cargar el dataset

data <- read.csv("D:/Diplomado UPB/Prediccion Analitica/Bases de datos/Bases de datos/Lung_Cancer_Dataset.csv")


# Mostrar estructura de datos
glimpse(data)

# Convertir PULMONARY_DISEASE a binaria (1 = YES, 0 = NO)
data$PULMONARY_DISEASE <- ifelse(data$PULMONARY_DISEASE == "YES", 1, 0)

# Eliminar variables no significativas
data <- data %>% select(-AGE, -GENDER, -LONG_TERM_ILLNESS, -ALCOHOL_CONSUMPTION, -OXYGEN_SATURATION)

# Normalizar variables numéricas restantes
data <- data %>% mutate(ENERGY_LEVEL = scale(ENERGY_LEVEL))

# Convertir la variable de respuesta a factor (importante para Random Forest)
data$PULMONARY_DISEASE <- as.factor(data$PULMONARY_DISEASE)

#################################
#### DIVISIÓN TRAIN / TEST ####
#################################
set.seed(42)  # Reproducibilidad
trainIndex <- createDataPartition(data$PULMONARY_DISEASE, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

######################################
#### AJUSTE DEL MODELO LOGÍSTICO ####
######################################
logit_model <- glm(PULMONARY_DISEASE ~ ., data = train_data, family = binomial)

# Resumen del modelo
summary(logit_model)

######################################
#### AJUSTE DEL MODELO RANDOM FOREST ####
######################################
set.seed(42)

# Entrenar Random Forest asegurando que prediga clases binarias
rf_model <- randomForest(PULMONARY_DISEASE ~ ., 
                         data = train_data, 
                         ntree = 500, 
                         mtry = 3, 
                         importance = TRUE)

# Importancia de las variables
importance(rf_model)
varImpPlot(rf_model)

#################################
#### EVALUACIÓN DEL MODELO ####
#################################

# Predicciones en test con Regresión Logística
test_data$pred_prob_logit <- predict(logit_model, newdata = test_data, type = "response")
test_data$pred_class_logit <- ifelse(test_data$pred_prob_logit > 0.3, 1, 0)  # Ajuste del umbral a 0.3

# Predicciones en test con Random Forest (asegurando predicción de clases binarias)
test_data$pred_class_rf <- predict(rf_model, newdata = test_data, type = "class")

# Matriz de Confusión - Regresión Logística
conf_matrix_logit <- table(Real = test_data$PULMONARY_DISEASE, Predicho = test_data$pred_class_logit)
print(conf_matrix_logit)

# Matriz de Confusión - Random Forest
conf_matrix_rf <- table(Real = test_data$PULMONARY_DISEASE, Predicho = test_data$pred_class_rf)
print(conf_matrix_rf)

# Métricas de evaluación - Regresión Logística
accuracy_logit <- sum(diag(conf_matrix_logit)) / sum(conf_matrix_logit)
precision_logit <- conf_matrix_logit[2,2] / sum(conf_matrix_logit[,2])
recall_logit <- conf_matrix_logit[2,2] / sum(conf_matrix_logit[2,])

# Métricas de evaluación - Random Forest
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
precision_rf <- conf_matrix_rf[2,2] / sum(conf_matrix_rf[,2])
recall_rf <- conf_matrix_rf[2,2] / sum(conf_matrix_rf[2,])

# Mostrar métricas
cat("Regresión Logística - Accuracy:", round(accuracy_logit, 3), 
    " Precision:", round(precision_logit, 3), 
    " Recall:", round(recall_logit, 3), "\n")

cat("Random Forest - Accuracy:", round(accuracy_rf, 3), 
    " Precision:", round(precision_rf, 3), 
    " Recall:", round(recall_rf, 3), "\n")

#################################
#### CURVA ROC Y AUC ####
#################################

# Curva ROC - Regresión Logística
roc_curve_logit <- roc(as.numeric(as.character(test_data$PULMONARY_DISEASE)), test_data$pred_prob_logit)
plot(roc_curve_logit, col = "blue", main = "Curva ROC - Regresion Logistica")
auc_logit <- auc(roc_curve_logit)
cat("Area bajo la curva (AUC) - Regresion Logistica:", round(auc_logit, 4), "\n")

# Curva ROC - Random Forest
roc_curve_rf <- roc(as.numeric(as.character(test_data$PULMONARY_DISEASE)), as.numeric(as.character(test_data$pred_class_rf)))
plot(roc_curve_rf, col = "red", main = "Curva ROC - Random Forest")
auc_rf <- auc(roc_curve_rf)
cat("AUC - Random Forest:", round(auc_rf, 4), "\n")