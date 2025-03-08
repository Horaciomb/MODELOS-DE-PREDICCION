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


# Ejercicio 2: PCA y Varianza Explicada -----------------------------------
pca <- prcomp(datos_estandar, scale = FALSE) # Ya estandarizados

# Scree Plot y Pareto
varianza_explicada <- pca$sdev^2 / sum(pca$sdev^2)
varianza_acumulada <- cumsum(varianza_explicada)

cat("\nEjercicio 2 - Varianza Explicada:\n")
print(data.frame(
  Componente = paste0("PC",1:4),
  Varianza = round(varianza_explicada,4),
  Acumulada = round(varianza_acumulada,4)
))

# Gráficos
par(mfrow = c(1,2))
# Scree Plot
barplot(varianza_explicada,
        main = "Scree Plot",
        names.arg = paste0("PC",1:4),
        ylab = "Varianza Explicada",
        col = "skyblue")

# Pareto
barplot(varianza_explicada,
        main = "Pareto de Varianza",
        names.arg = paste0("PC",1:4),
        ylab = "Proporción",
        col = "lightgreen")
lines(varianza_acumulada, type = "b", pch = 19, col = "red")
abline(h = 0.9, lty = 2, col = "blue")
par(mfrow = c(1,1))

# Componentes para 90% de varianza
n_componentes <- which(varianza_acumulada >= 0.9)[1]
cat("\nSe necesitan", n_componentes, "componentes para explicar +90% de varianza.\n\n")

# Ejercicio 3: Visualización de Componentes -------------------------------
# Biplot
cat("\nEjercicio 3 - Biplot:\n")
fviz_pca_biplot(pca,
                col.var = "darkred",
                col.ind = "darkblue",
                repel = TRUE,
                title = "Biplot PCA") +
  theme_minimal()

# Gráfico en nuevo espacio
cat("\nGráfico en Espacio de Componentes Principales:\n")
fviz_pca_ind(pca,
             col.ind = "contrib",
             gradient.cols = c("blue", "green", "red"),
             title = "Datos en PC1-PC2") +
  theme_minimal()

cat("\nExplicación Final:
- PC1 captura principalmente variabilidad en cilindrada y potencia (disp, hp)
- PC2 está relacionado con peso (wt)
- El biplot muestra cómo las variables originales contribuyen a las componentes principales
- Los vehículos se agrupan según características técnicas en el nuevo espacio reducido.")
