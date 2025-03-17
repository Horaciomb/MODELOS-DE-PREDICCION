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
library(ggplot2)

######################
####CARGA DE DATOS####
######################
data <- read.csv("D:/Diplomado UPB/Prediccion Analitica/Bases de datos/Bases de datos/Lung_Cancer_Dataset.csv")

# Mostrar estructura de datos
summary(data)
str(data)




#### Ananlisis Exploratorio de DATOS ####

# Contar la frecuencia de cada categoría
# Distribución de Género
ggplot(data, aes(x=factor(GENDER))) +
  geom_bar(fill="skyblue", color="black") +
  labs(title="Distribución de Género", x="Género", y="Frecuencia") +
  scale_x_discrete(labels=c("Femenino", "Masculino"))

# Distribución de Tabaquismo
ggplot(data, aes(x=factor(SMOKING))) +
  geom_bar(fill="lightgreen", color="black") +
  labs(title="Distribución de Tabaquismo", x="Tabaquismo", y="Frecuencia") +
  scale_x_discrete(labels=c("No", "Sí"))

# Distribución de Discoloración de los Dedos
ggplot(data, aes(x=factor(FINGER_DISCOLORATION))) +
  geom_bar(fill="lightcoral", color="black") +
  labs(title="Distribución de Discoloración de los Dedos", x="Discoloración de los Dedos", y="Frecuencia") +
  scale_x_discrete(labels=c("No", "Sí"))

# Distribución de Enfermedades Pulmonares
ggplot(data, aes(x=factor(PULMONARY_DISEASE))) +
  geom_bar(fill="lightcoral", color="black") +
  labs(title="Distribución de Enfermedades Pulmonares", x="Enfermedades Pulmonares", y="Frecuencia") +
  scale_x_discrete(labels=c("No", "Sí"))
table(data$PULMONARY_DISEASE) 

# Comparar nivel de energía por género
boxplot(ENERGY_LEVEL ~ GENDER, data = data, main = "Nivel de energía por género", xlab = "Género", ylab = "Nivel de energía")

# Comparar saturación de oxígeno por tabaquismo
boxplot(OXYGEN_SATURATION ~ SMOKING, data = data, main = "Saturación de oxígeno por tabaquismo", xlab = "Tabaquismo", ylab = "Saturación de oxígeno")

# Verificar valores faltantes
colSums(is.na(data))

## Distribución de variables numéricas
# Histograma de la edad

hist(data$AGE, main = "Distribución de Edad", xlab = "Edad", col = "blue")

# Histograma de nivel de energía
hist(data$ENERGY_LEVEL, main = "Distribución de Nivel de Energía", xlab = "Nivel de Energía", col = "green")


#################################
#### PREPROCESAMIENTO DE DATOS ###
#################################
# Convertir PULMONARY_DISEASE a binaria (1 = YES, 0 = NO)
data$PULMONARY_DISEASE <- ifelse(data$PULMONARY_DISEASE == "YES", 1, 0)

# Convertir variables categóricas a factores
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Normalizar variables numéricas
data <- data %>%
  mutate(AGE = scale(AGE),
         ENERGY_LEVEL = scale(ENERGY_LEVEL),
         OXYGEN_SATURATION = scale(OXYGEN_SATURATION))

###############################
####DIVISIÓN TRAIN / TEST####
###############################
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

#################################
#### EVALUACIÓN DEL MODELO ####
#################################
# Predicciones en test
test_data$pred_prob <- predict(logit_model, newdata = test_data, type = "response")
test_data$pred_class <- ifelse(test_data$pred_prob > 0.5, 1, 0)

# Matriz de Confusión
conf_matrix <- table(Real = test_data$PULMONARY_DISEASE, Predicho = test_data$pred_class)
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
roc_curve <- roc(test_data$PULMONARY_DISEASE, test_data$pred_prob)
plot(roc_curve, col = "blue", main = "Curva ROC")
auc(roc_curve)