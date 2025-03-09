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

# EJERCICIO 2: PCA y Varianza Explicada -----------------------------------
# Aplicar PCA
pca <- prcomp(datos_estandar, scale = FALSE)

# Scree Plot (Base R)
varianza <- pca$sdev^2
prop_var <- varianza/sum(varianza)
plot(prop_var, type = "b", 
     main = "Scree Plot", 
     xlab = "Componente Principal",
     ylab = "Proporción de Varianza Explicada")

# Gráfico de Pareto
df_var <- data.frame(
  PC = paste0("PC", 1:4),
  Individual = prop_var,
  Acumulada = cumsum(prop_var)
)

ggplot(df_var, aes(x = PC)) +
  geom_col(aes(y = Individual), fill = "steelblue") +
  geom_line(aes(y = Acumulada, group = 1), color = "red", size = 1) +
  geom_point(aes(y = Acumulada), color = "red", size = 2) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "darkgreen") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Varianza Explicada - Diagrama de Pareto",
       y = "Proporción de Varianza") +
  theme_minimal()

# Componentes para 90% de varianza
n_comp <- which(cumsum(prop_var) >= 0.9)[1]
cat("Se necesitan", n_comp, "componentes para explicar el 90% de la varianza")

# EJERCICIO 3: Visualización ----------------------------------------------
# Biplot con etiquetas
autoplot(pca, data = mtcars, 
         loadings = TRUE, loadings.label = TRUE,
         loadings.label.size = 4, loadings.label.colour = "darkblue",
         label = TRUE, label.size = 3) +
  ggtitle("Biplot - Componentes Principales") +
  theme_minimal()

# Gráfico de componentes principales
df_pca <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  Modelo = rownames(mtcars)
)

ggplot(df_pca, aes(PC1, PC2, label = Modelo)) +
  geom_point(color = "firebrick", size = 3) +
  geom_text(hjust = 0, vjust = 1.5, size = 3) +
  labs(title = "Espacio reducido por PCA",
       x = paste0("PC1 (", round(prop_var[1]*100, 1), "%)"),
       y = paste0("PC2 (", round(prop_var[2]*100, 1), "%)")) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

# Explicación de Resultados -----------------------------------------------
cat("\nINTERPRETACIÓN FINAL:
1. Correlaciones: Las variables originales están altamente correlacionadas (r > 0.8)
2. Varianza Explicada: PC1 explica", round(prop_var[1]*100, 1), "% de la varianza
3. Biplot: 
   - Vehículos con mayor peso (wt) y cilindrada (disp) están a la izquierda
   - Coches deportivos (Maserati Bora) tienen valores altos en PC1
   - El peso (wt) es la variable que más contribuye a PC1
4. Componentes: Con 2 componentes explicamos", round(cumsum(prop_var)[2]*100, 1), "% de la varianza")
