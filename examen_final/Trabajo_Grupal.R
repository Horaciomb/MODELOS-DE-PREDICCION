library(tidyverse)
#install.packages("ggplot2")
#install.packages("cluster")
#install.packages("dbscan")
#install.packages("isotree")

library(tidyverse)
library(ggplot2)
library(cluster)
library(dbscan)
library(isotree)
####################
####CARGAR DATOS####
####################
data <- read.csv("D:/Diplomado UPB/Prediccion Analitica/Bases de datos/Bases de datos/Olympics_(1896-2024).csv")
# Mostrar las primeras filas del conjunto de datos

head(data, n = 5)
###########################
####INSPECCION DE DATOS####
###########################
# Resumen de las variables en el conjunto de datos

summary(data)

# Ver la estructura del conjunto de datos
str(data)
# Seleccionar columnas específicas
data_select <- data %>% 
  select(NOC, Rank, Year)

# Convertir Rank a numérico
data$Rank <- as.numeric(data$Rank)  

