# Ejercicio 1: Regresión Lineal Simple --------------------------------------------------
modelo1 <- lm(mpg ~ wt, data = mtcars)
cat("EJERCICIO 1 - Regresión Simple (mpg ~ wt):\n")
print(summary(modelo1))

cat("\nExplicación Ejercicio 1:
- Coeficientes: La ecuación del modelo es mpg =", coef(modelo1)[1], "+", coef(modelo1)[2], "*wt
- Interpretación: Por cada 1000 lbs de aumento en peso, el mpg disminuye aproximadamente", abs(round(coef(modelo1)[2],1)), "unidades.
- R²:", round(summary(modelo1)$r.squared, 2), "indica que el peso explica el", round(summary(modelo1)$r.squared*100, 1), "% de la variabilidad en mpg.
- p-valor: <2e-16 (significativo) confirma una relación fuerte entre peso y mpg.\n\n")

# Ejercicio 2: Modelo Polinomial (Grado 2) ----------------------------------------------
modelo2 <- lm(mpg ~ wt + I(wt^2), data = mtcars)
mtcars_ordenado <- mtcars[order(mtcars$wt), ]

cat("EJERCICIO 2 - Resumen Modelo Polinomial:\n")
print(summary(modelo2))

plot(mtcars$wt, mtcars$mpg, 
     main = "Regresión Polinomial (Grado 2)",
     xlab = "Peso (wt)", ylab = "mpg",
     pch = 19, col = "darkblue")
lines(mtcars_ordenado$wt, predict(modelo2, mtcars_ordenado), 
      col = "red", lwd = 2)

cat("\nExplicación Ejercicio 2:
- El modelo incluye wt y wt². Ambos términos son significativos (p < 0.05).
- La forma parabólica en la gráfica sugiere que la relación peso-mpg no es estrictamente lineal.
- El R² ajustado aumenta a", round(summary(modelo2)$adj.r.squared, 2), "indicando mejor ajuste que el modelo lineal simple.\n\n")



