################################
#####INTRODUCCION A RSTUDIO#####
################################

################
####PAQUETES####
################
# Instalar paquetes si no están instalados
#install.packages("tidyverse")

# Cargar los paquetes necesarios
library(tidyverse)


####################
####CARGAR DATOS####
####################
# Cargar el conjunto de datos 'mtcars'
data("mtcars")

# Mostrar las primeras filas del conjunto de datos
head(mtcars, n = 5)

###########################
####INSPECCION DE DATOS####
###########################
# Resumen de las variables en el conjunto de datos
summary(mtcars)

# Ver la estructura del conjunto de datos
str(mtcars)

#############################
####MANIPULACION DE DATOS####
#############################
# Seleccionar columnas específicas
mtcars_select <- mtcars %>%
  select(mpg, hp, wt)

# Filtrar las observaciones donde los caballos de fuerza (hp) son mayores a 100
mtcars_filter <- mtcars %>%
  filter(hp > 100)

# Crear una nueva variable: relación peso a potencia
mtcars <- mtcars %>%
  mutate(weight_to_hp = wt / hp)

# Mostrar las primeras filas del nuevo conjunto de datos
head(mtcars)

#####################################
####AGRUPACION Y RESUMEN DE DATOS####
#####################################
# Agrupar por número de cilindros y calcular el promedio de millas por galón
mtcars_grouped <- mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean_mpg = mean(mpg))

# Mostrar el resumen
print(mtcars_grouped)


####################
####OTRO EJEMPLO####
####################

# Cargar el archivo CSV (ajusta la ruta al archivo según donde esté guardado)
gym_data <- read.csv("G:/My Drive/Modelos de prediccion - Febrero 2025/Bases de datos/gym_members_exercise_tracking.csv")

# Mostrar las primeras filas del conjunto de datos
head(gym_data)

# Resumen de las variables en el conjunto de datos
summary(gym_data)

# Ver la estructura del conjunto de datos
str(gym_data)

# Seleccionar columnas específicas
gym_data_select <- gym_data %>% 
  select(Age, Gender, Weight..kg.)

# Mostrar las primeras filas del nuevo conjunto de datos
head(gym_data_select)

# Filtrar las observaciones donde la duración del ejercicio es mayor a 30 minutos
gym_data_filtered <- gym_data %>%
  filter(Session_Duration..hours. > 0.5)

# Mostrar las primeras filas del conjunto de datos filtrado
head(gym_data_filtered)

# Crear una nueva columna: relación calorías quemadas por minuto de ejercicio
gym_data <- gym_data %>%
  mutate(calorias_por_hora = Calories_Burned  / Session_Duration..hours.)

# Mostrar las primeras filas del conjunto de datos con la nueva columna
head(gym_data)

# Agrupar por genero y calcular la duración promedio del ejercicio y las calorías quemadas
gym_data_grouped <- gym_data %>% 
  group_by(Gender) %>% 
  summarise(mean_duration = mean(Session_Duration..hours.),
            mean_calories = mean(Calories_Burned ))

# Mostrar el resumen agrupado
print(gym_data_grouped)

# Agrupar por tipo de ejercicio y calcular la duración promedio y las calorias
gym_data_grouped_2 <- gym_data %>%
  group_by(Workout_Type) %>%
  summarise(mean_duration = mean(Session_Duration..hours.),
            mean_calories = mean(Calories_Burned),
            mean_cal_hora = mean(calorias_por_hora))

#Visualizar
head(gym_data_grouped_2)