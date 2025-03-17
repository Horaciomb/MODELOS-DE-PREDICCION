library(tidyverse)
#install.packages("ggplot2")
#install.packages("cluster")
#install.packages("dbscan")
#install.packages("isotree")
#install.packages("corrplot")
library(corrplot)

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


# Datos faltantes
sum(is.na(data))

data <- data %>%
  filter(!Year %in% c(1916, 1940, 1944))


ggplot(data, aes(x = Year, y = Total, color = NOC)) +  
  geom_line() +  
  labs(title = "Evolución del Total de Medallas por País", x = "Año", y = "Total de Medallas")  



data_long <- pivot_longer(data, cols = c(Gold, Silver, Bronze), names_to = "Medal")  
ggplot(data_long, aes(x = Medal, y = value, fill = NOC)) +  
  geom_boxplot() +  
  theme(legend.position = "none")  


cor_matrix <- cor(data[, c("Gold", "Silver", "Bronze", "Total")])  
corrplot(cor_matrix, method = "number")  