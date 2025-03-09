# ANÁLISIS DE COMPONENTES PRINCIPALES (PCA) - mtcars
# PAQUETES ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggfortify)

# EJERCICIO 1: Exploración y Preprocesamiento -----------------------------
# Cargar y preparar datos
data(mtcars)
vars <- c("cyl", "disp", "hp", "wt")
datos <- mtcars[, vars]

# Estandarización
datos_estandar <- scale(datos) %>% as.data.frame()
colnames(datos_estandar) <- c("cyl", "disp", "hp", "wt")

# Matriz de correlación
cor_matrix <- cor(datos_estandar)

# Heatmap con ggplot
melted_cor <- melt(cor_matrix)
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  labs(title = "Mapa de Calor de Correlación",
       x = "", y = "") +
  theme_minimal() +
  theme

