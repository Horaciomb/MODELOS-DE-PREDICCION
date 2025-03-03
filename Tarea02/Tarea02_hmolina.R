# Ejercicio 1: Regresión Lineal Simple --------------------------------------------------
modelo1 <- lm(mpg ~ wt, data = mtcars)
cat("EJERCICIO 1 - Regresión Simple (mpg ~ wt):\n")
print(summary(modelo1))

cat("\nExplicación Ejercicio 1:
- Coeficientes: La ecuación del modelo es mpg =", coef(modelo1)[1], "+", coef(modelo1)[2], "*wt
- Interpretación: Por cada 1000 lbs de aumento en peso, el mpg disminuye aproximadamente", abs(round(coef(modelo1)[2],1)), "unidades.
- R²:", round(summary(modelo1)$r.squared, 2), "indica que el peso explica el", round(summary(modelo1)$r.squared*100, 1), "% de la variabilidad en mpg.
- p-valor: <2e-16 (significativo) confirma una relación fuerte entre peso y mpg.\n\n")
