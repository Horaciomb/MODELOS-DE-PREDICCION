#################
####LIBRERIAS####
#################
#install.packages("factoextra")
library(dplyr)
library(corrplot)
library(ggplot2)
library(factoextra)

# Ejercicio 1: Exploración y Preprocesamiento -----------------------------
# Seleccionar y estandarizar variables
datos <- mtcars %>% select(cyl, disp, hp, wt)
datos_estandar <- scale(datos) # Estandarización (media=0, sd=1)

# Matriz de correlación
matriz_cor <- cor(datos_estandar)

# Mapa de calor
cat("\nEjercicio 1 - Mapa de Calor de Correlaciones:\n")
corrplot(matriz_cor, 
         method = "color",
         addCoef.col = "black",
         tl.col = "darkblue",
         title = "Matriz de Correlación")

cat("\nExplicación Ejercicio 1:
- Las variables muestran fuertes correlaciones (ej: disp vs hp = 0.79)
- Esto justifica el uso de PCA para reducir dimensionalidad.\n\n")