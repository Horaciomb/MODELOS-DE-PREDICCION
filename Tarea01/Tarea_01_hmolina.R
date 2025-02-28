library(tidyverse)
library(dplyr)

data("mtcars")

# Ejercicio 1: Selección de columnas
ej1 <- mtcars %>% select(mpg, hp, wt)
cat("Ejercicio 1 - Primeras 6 filas:\n")
print(head(ej1))

# Ejercicio 2: Filtrado de observaciones
ej2 <- mtcars %>% filter(hp > 100, wt < 3)
cat("\nEjercicio 2 - Autos con hp > 100 y wt < 3:\n")
print(head(ej2)
      
# Ejercicio 3: Creación de nuevas variables
ej3 <- mtcars %>% mutate(efficiency_ratio = mpg/wt)
cat("\nEjercicio 3 - Datos con ratio de eficiencia:\n")
print(head(ej3))


# Ejercicio 4: Agrupación y resumen
ej4 <- mtcars %>% 
  group_by(cyl) %>% 
  summarise(
    Promedio_mpg = mean(mpg),
    Promedio_hp = mean(hp)
  )
cat("\nEjercicio 4 - Resumen por cilindros:\n")
print(ej4)



# Ejercicio 5: Ordenación de datos
ej5 <- mtcars %>% arrange(desc(mpg))
cat("\nEjercicio 5 - Datos ordenados por mpg:\n")
print(head(ej5))