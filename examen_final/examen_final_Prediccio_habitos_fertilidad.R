# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
# Cargar librería necesaria
library(fastDummies)

# Cargar datos
data <- read.csv("D:/Diplomado UPB/Prediccion Analitica/Bases de datos/Bases de datos/fertility.csv")

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
data <- dummy_cols(data, select_columns = categorical_columns, remove_selected_columns = TRUE)

# Ver la estructura del nuevo DataFrame con las variables dummies
str(data)


