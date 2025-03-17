# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(forecast)
if (!require(randomForest)) install.packages("randomForest", dependencies = TRUE)

# Cargar la librería
library(randomForest)

# Cargar librería necesaria
library(fastDummies)

# Cargar datos
data <- read.csv("D:/Diplomado UPB/Prediccion Analitica/Bases de datos/Bases de datos/Fertility_Dataset.csv")

# Mostrar estructura y resumen
str(data)
summary(data)


# Limpiando datos 
# filtramos los que tengas mas de 24 horas setandos al día (correcion por error en un eleemento)
data <- data %>%  
  filter(Number.of.hours.spent.sitting.per.day <= 24)


# Verificar valores únicos en la columna Diagnosis
unique(data$Diagnosis)

# Convertir Diagnosis a binaria (1 = Altered, 0 = Normal)
data$Diagnosis <- ifelse(data$Diagnosis == "Altered", 1, 0)



# Lista de columnas categóricas que deseas convertir a dummies
categorical_columns <- c("Smoking.habit", "Season", "Frequency.of.alcohol.consumption", 
                         "Childish.diseases", "Accident.or.serious.trauma", 
                         "Surgical.intervention", "High.fevers.in.the.last.year")


# Crear variables dummies y eliminar las columnas originales
data <- dummy_cols(data, select_columns = categorical_columns, remove_selected_columns = FALSE)

# Ver la estructura del nuevo DataFrame con las variables dummies
str(data)

### ANALISIS EXPLORATORIO DE DATOS

library(ggplot2)

# Gráfico de barras para visualizar la distribución de Diagnosis
ggplot(data, aes(x = factor(Diagnosis, labels = c("Normal", "Altered")))) +  
  geom_bar(fill = "skyblue") +  
  labs(title = "Distribución de Diagnóstico de Fertilidad", x = "Diagnóstico", y = "Frecuencia") +
  theme_minimal()

# Relacion entre edad y diagnostico
ggplot(data, aes(x = factor(Diagnosis, labels = c("Normal", "Altered")), y = Age)) +  
  geom_boxplot(fill = "orange", alpha = 0.7) +  
  labs(title = "Edad vs Diagnóstico", x = "Diagnóstico", y = "Edad") +  
  theme_minimal()

## Impacto del hábito de fumar
ggplot(data, aes(x = Smoking.habit, fill = factor(Diagnosis, labels = c("Normal", "Altered")))) +  
  geom_bar(position = "dodge") +  
  scale_fill_manual(values = c("blue", "red")) +  
  labs(title = "Hábito de Fumar vs Diagnóstico", x = "Hábito de Fumar", y = "Frecuencia", fill = "Diagnóstico") +  
  theme_minimal()


## 3.4. Relación entre Horas Sentado y Diagnóstico
ggplot(data, aes(x = factor(Diagnosis, labels = c("Normal", "Altered")), y = Number.of.hours.spent.sitting.per.day)) +  
  geom_boxplot(fill = "purple", alpha = 0.7) +  
  labs(title = "Horas Sentado vs Diagnóstico", x = "Diagnóstico", y = "Horas Sentado por Día") +  
  theme_minimal()



### 4. Modelado Predictivo

### 4.1. División del Conjunto de Datos
set.seed(42)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

## 4.2. Regresión Logística
model_logit <- glm(Diagnosis ~ Age + Smoking.habit_daily + Number.of.hours.spent.sitting.per.day,  
                   data = train_data, family = "binomial")  
summary(model_logit)

## 4.3. Random Forest
set.seed(42)  
model_rf <- randomForest(Diagnosis ~ ., data = train_data)  
print(model_rf)


colnames(train_data)
sum(is.na(train_data$Frequency.of.alcohol.consumption_every.day))

