####################################
#####PREDICCIONES - EXTENSIONES#####
####################################

#################
####LIBRERIAS####
#################
#Instalamos las siguientes librerias si no las tenemos
#install.packages("forescast")
#install.packages("tseries)
#install.packages("plm")
#install.packages("prophet)

# Instalar y cargar librerías
library(forecast)
library(tseries)
library(plm)
library(prophet)
library(ggplot2)
library(dplyr)
library(zoo)


########################
####SERIES DE TIEMPO####
########################
# Cargar y visualizar la serie temporal de ejemplo
data("AirPassengers")
ts_data <- AirPassengers
plot(ts_data)

# Descomponer la serie temporal aditiva
decomposed_ts <- decompose(ts_data,
                           type = "additive")
plot(decomposed_ts)

# Descomponer la serie temporal multiplicativa
decomposed_ts <- decompose(ts_data,
                           type = "multiplicative")
plot(decomposed_ts)

# Ajustar un modelo ARIMA
fit_arima <- auto.arima(ts_data)
summary(fit_arima)

# Predicción a 48 periodos futuros
forecast_values <- forecast(fit_arima, h = 48)
plot(forecast_values)


######################
####MODELO PROPHET####
######################
# Preparar datos para Prophet
ts_data_df <- data.frame(
  ds = seq.Date(from = as.Date("1949-01-01"),
                by = "month",
                length.out = length(ts_data)),
  y = as.numeric(ts_data)
)

# Ajustar el modelo Prophet
prophet_model <- prophet(ts_data_df)

# Crear un marco de datos para predicciones futuras
future <- make_future_dataframe(prophet_model,
                                periods = 48,
                                freq = "month")

# Realizar predicciones
forecast_prophet <- predict(prophet_model,
                            future)

# Ver las primeras filas de las predicciones
head(forecast_prophet)

# Graficar predicciones
plot(prophet_model, forecast_prophet)

# Graficar componentes (tendencia y estacionalidad)
prophet_plot_components(prophet_model,
                        forecast_prophet)


#Probemos con argumentos diferentes
prophet_model <- prophet(ts_data_df,
                         seasonality.mode = "multiplicative")

# Crear un marco de datos para predicciones futuras
future <- make_future_dataframe(prophet_model,
                                periods = 48,
                                freq = "month")

# Realizar predicciones
forecast_prophet <- predict(prophet_model,
                            future)

# Ver las primeras filas de las predicciones
head(forecast_prophet)

# Graficar predicciones
plot(prophet_model, forecast_prophet)

# Graficar componentes (tendencia y estacionalidad)
prophet_plot_components(prophet_model,
                        forecast_prophet)


#############################
####COMPARACION IN-SAMPLE####
#############################
# Obtener predicciones de Prophet y ARIMA para los datos reales
pred_arima <- as.numeric(fitted(fit_arima))
pred_prophet <- forecast_prophet$yhat[1:length(ts_data)]

# Calcular el MSE
mse_arima <- mean((as.numeric(ts_data) - pred_arima)^2)
mse_prophet <- mean((as.numeric(ts_data) - pred_prophet)^2)

cat("MSE ARIMA:", mse_arima, "\n")
cat("MSE Prophet:", mse_prophet, "\n")

## Creamos una base con los valores combinados
# Valores originales
original_values <- data.frame(
  Date = seq.Date(from = as.Date("1949-01-01"),
                  by = "month",
                  length.out = length(ts_data)),
  Actual = as.numeric(ts_data)
)

# Agregar predicciones del modelo ARIMA (fitted values)
original_values$ARIMA_Predictions <- fitted(fit_arima)

# Agregar predicciones de Prophet
forecast_prophet_df <- forecast_prophet[, c("ds", "yhat")]
names(forecast_prophet_df) <- c("Date", "Prophet_Predictions")

# Asegurarse de que 'Date' esté en formato fecha
forecast_prophet_df$Date <- as.Date(forecast_prophet_df$Date)

# Combinar las predicciones de Prophet al dataframe principal
final_df <- merge(original_values,
                  forecast_prophet_df,
                  by = "Date",
                  all.x = TRUE)

# Mostrar los primeros valores del dataframe combinado
head(final_df)

# Crear el gráfico
ggplot(final_df, aes(x = Date)) +
  geom_line(aes(y = Actual,
                color = "Valores Originales"),
            size = 1,
            alpha = 0.6,
            linetype = "dotted") +
  geom_line(aes(y = ARIMA_Predictions,
                color = "Predicciones ARIMA"),
            size = 1,
            alpha = 0.6) +
  geom_line(aes(y = Prophet_Predictions,
                color = "Predicciones Prophet"),
            size = 1,
            alpha = 0.6) +
  labs(
    title = "Comparación de Valores Originales y Predicciones",
    x = "Fecha",
    y = "Cantidad de Pasajeros",
    color = "Leyenda"
  ) +
  scale_color_manual(values = c("Valores Originales" = "black",
                                "Predicciones ARIMA" = "blue",
                                "Predicciones Prophet" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  )

##############################
####COMPARACION OUT-SAMPLE####
##############################
# Ajustar el modelo ARIMA a los datos originales
fit_arima <- auto.arima(ts_data)

# Realizar predicciones a 48 periodos futuros
forecast_arima <- forecast(fit_arima, h = 48)

# Obtener la fecha inicial de la serie temporal
start_date <- as.Date(paste(start(ts_data)[1], start(ts_data)[2], "01", sep = "-"))

# Crear un dataframe para las predicciones ARIMA
arima_forecast_df <- data.frame(
  Date = seq(
    from = start_date + months(length(ts_data)),
    by = "month",
    length.out = 48
  ),
  ARIMA_Predictions = as.numeric(forecast_arima$mean)
)

# Crear un dataframe para predicciones futuras con Prophet
future_prophet <- make_future_dataframe(prophet_model,
                                        periods = 48,
                                        freq = "month")

# Generar las predicciones de Prophet
forecast_prophet <- predict(prophet_model,
                            future_prophet)

# Filtrar las predicciones futuras
prophet_forecast_df <- forecast_prophet %>%
  filter(ds > max(ts_data_df$ds)) %>%
  select(ds, yhat) %>%
  rename(Date = ds, Prophet_Predictions = yhat)

# Asegurarse de que 'Date' esté en formato fecha
forecast_prophet_df$Date <- as.Date(forecast_prophet_df$Date)

# Crear el dataframe final combinando ambas predicciones
combined_forecast_df <- left_join(arima_forecast_df,
                              prophet_forecast_df,
                              by = "Date")

# Agregar los valores reales para la comparación
original_data_df <- data.frame(
  Date = as.Date(as.yearmon(time(ts_data))),
  Actual = as.numeric(ts_data)
)

final_df <- bind_rows(
  original_data_df %>% filter(Date <= max(original_data_df$Date)),
  combined_forecast_df
)

#Graficar
ggplot(final_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Valores Originales"),
            size = 1) +
  geom_line(data = combined_forecast_df,
            aes(y = ARIMA_Predictions,
                color = "ARIMA Predicciones"),
            size = 1,
            alpha = 0.7) +
  geom_line(data = combined_forecast_df,
            aes(y = Prophet_Predictions,
                color = "Prophet Predicciones"),
            size = 1,
            alpha = 0.7) +
  labs(
    title = "Predicciones Out-of-Sample: ARIMA vs Prophet",
    x = "Fecha",
    y = "Pasajeros",
    color = "Leyenda"
  ) +
  scale_color_manual(values = c(
    "Valores Originales" = "black",
    "ARIMA Predicciones" = "blue",
    "Prophet Predicciones" = "red"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  )

######################
####DATOS DE PANEL####
######################
# Cargar los datos de ejemplo (dataset "Produc" de plm)
data("Produc", package = "plm")

# Ver las primeras filas del conjunto de datos
head(Produc)

# Ajustar un modelo de efectos fijos
fixed_model <- plm(gsp ~ pcap + unemp + hwy + water,
                   data = Produc,
                   index = c("state", "year"),
                   model = "within")

# Mostrar el resumen del modelo
summary(fixed_model)

# Ajustar un modelo de efectos aleatorios
random_model <- plm(gsp ~ pcap + unemp + hwy + water,
                    data = Produc, index = c("state", "year"),
                    model = "random")

# Mostrar el resumen del modelo
summary(random_model)

# Realizar la prueba de Hausman
hausman_test <- phtest(fixed_model, random_model)

# Mostrar los resultados de la prueba de Hausman
print(hausman_test)