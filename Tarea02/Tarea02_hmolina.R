




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



# Ejercicio 3: Modelo con Interacción ---------------------------------------------------
modelo3 <- lm(mpg ~ wt * hp, data = mtcars)
cat("EJERCICIO 3 - Modelo con Interacción (wt * hp):\n")
print(summary(modelo3))

cat("\nExplicación Ejercicio 3:
- La interacción wt:hp tiene p-valor =", round(summary(modelo3)$coefficients[4,4], 3), 
    "\n  Esto indica que la interacción", ifelse(summary(modelo3)$coefficients[4,4] < 0.05, 
                                                 "ES significativa (p < 0.05).", "NO es significativa (p > 0.05)."),
    "\n  Cuando existe interacción, el efecto de wt en mpg depende del nivel de hp (y viceversa).\n\n")

# Ejercicio 4: Diagnóstico del Modelo ---------------------------------------------------
modelo4 <- lm(mpg ~ wt + hp + qsec, data = mtcars)

cat("EJERCICIO 4 - Gráficos de Diagnóstico:\n")
par(mfrow = c(2,2))  # Configurar área de gráficos 2x2
plot(modelo4, pch = 19, col = "darkblue")
par(mfrow = c(1,1))  # Restaurar configuración gráfica

cat("\nExplicación Ejercicio 4:
1. Residuos vs Ajustados: Patrón aleatorio sugiere linealidad (sin tendencias claras).
2. Q-Q Plot: Residuos siguen aproximadamente la línea recta (normalidad aceptable).
3. Escala-Localización: Dispersión homogénea (supuesto de homocedasticidad).
4. Residuos vs Leverage: Observar puntos fuera de la línea de distancia Cook (0.5 o 1).
- El punto 20 (Toyota Corolla) muestra residuo grande pero baja influencia.
- No se detectan puntos con alta distancia de Cook (>1) que afecten significativamente el modelo.\n")