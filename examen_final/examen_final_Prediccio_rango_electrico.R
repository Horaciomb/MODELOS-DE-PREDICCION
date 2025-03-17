# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)

# Cargar datos
data_vehicule <- read.csv("D:/Diplomado UPB/Prediccion Analitica/Bases de datos/Bases de datos/Electric_Vehicle_Population_Data.csv")

# Mostrar estructura y resumen
str(data_vehicule)
summary(data_vehicule)


### Limpiza de Datos
# Eliminar filas donde Electric.Range es 0
data_vehicule <- data_vehicule %>% filter(Electric.Range > 0)

# Eliminar columnas irrelevantes
data_vehicule <- data_vehicule %>% select(-c(VIN..1.10., DOL.Vehicle.ID, X2020.Census.Tract))

# Imputar valores faltantes con la moda en Postal.Code y Legislative.District
mode_impute <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_vehicule$Postal.Code[is.na(data_vehicule$Postal.Code)] <- mode_impute(data_vehicule$Postal.Code)
data_vehicule$Legislative.District[is.na(data_vehicule$Legislative.District)] <- mode_impute(data_vehicule$Legislative.District)

## Tranformacion de variables
# Extraer latitud y longitud de Vehicle.Location
data <- data %>%
  mutate(
    Longitude = as.numeric(gsub(".*\\(([-0-9\\.]+) .*", "\\1", Vehicle.Location)),
    Latitude = as.numeric(gsub(".* ([-0-9\\.]+)\\)", "\\1", Vehicle.Location))
  ) %>%
  select(-Vehicle.Location)  # Eliminar columna original

# Crear variables dummy para Electric.Vehicle.Type y Make
data <- data %>%
  mutate(Electric.Vehicle.Type = as.factor(Electric.Vehicle.Type),
         Make = as.factor(Make)) %>%
  mutate_at(vars(Electric.Vehicle.Type, Make), as.character) %>%
  mutate_at(vars(Electric.Vehicle.Type, Make), as.factor)

# Crear variables dummy
data <- cbind(data, model.matrix(~Electric.Vehicle.Type + Make - 1, data = data))



# Extraer latitud y longitud de Vehicle.Location
data <- data %>%
  mutate(
    Longitude = as.numeric(gsub(".*\\(([-0-9\\.]+) .*", "\\1", Vehicle.Location)),
    Latitude = as.numeric(gsub(".* ([-0-9\\.]+)\\)", "\\1", Vehicle.Location))
  ) %>%
  select(-Vehicle.Location)  # Eliminar columna original

# Crear variables dummy para Electric.Vehicle.Type y Make
data <- data %>%
  mutate(Electric.Vehicle.Type = as.factor(Electric.Vehicle.Type),
         Make = as.factor(Make)) %>%
  mutate_at(vars(Electric.Vehicle.Type, Make), as.character) %>%
  mutate_at(vars(Electric.Vehicle.Type, Make), as.factor)

# Crear variables dummy
data <- cbind(data, model.matrix(~Electric.Vehicle.Type + Make - 1, data = data))



ggplot(data, aes(x = Electric.Range)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribución del Rango Eléctrico", x = "Rango Eléctrico (millas)", y = "Frecuencia")
