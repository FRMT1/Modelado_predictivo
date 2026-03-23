library(forecast); library(ggplot2)


# Datos de ejemplo: serie de tiempo de ventas mensuales con estacionalidad y tendencia
set.seed(123)
n <- 120 # 10 años de datos mensuales
tiempo <- 1:n
tendencia <- 0.5 * tiempo
estacionalidad <- 10 * sin(2 * pi * tiempo / 12) # Ciclo anual
ruido <- rnorm(n, mean = 0, sd = 5)
ventas <- 50 + tendencia + estacionalidad + ruido

# Convertir a objeto ts (time series)
ts_ventas <- ts(ventas, start = c(2015, 1), frequency = 12)

# Visualizar la serie de tiempo
autoplot(ts_ventas) +
  ggtitle("Serie de Tiempo de Ventas Mensuales") +
  xlab("Año") +
  ylab("Ventas")

# Dividir en conjunto de entrenamiento y prueba
train_size <- length(ts_ventas) - 24 # Usar los últimos 24 meses (2 años) para prueba
train_ts <- window(ts_ventas, end = c(2022, 12)) # Hasta diciembre de 2022
test_ts <- window(ts_ventas, start = c(2023, 1))  # Desde enero de 2023

# Entrenar el modelo nnetar
# Puedes experimentar con los parámetros
# model_nnar <- nnetar(train_ts, P=12, size=10) # P=12 para estacionalidad, size para complejidad
model_nnar <- nnetar(train_ts) # nnetar es bastante bueno determinando los parámetros por defecto

print(model_nnar)

# Realizar pronósticos para el periodo de prueba
forecast_nnar <- forecast(model_nnar, h = length(test_ts))

# Visualizar los pronósticos
autoplot(ts_ventas) +
  autolayer(forecast_nnar, series = "Pronóstico NNAR", PI = FALSE) + # PI = FALSE para no mostrar intervalos de predicción
  autolayer(test_ts, series = "Datos Reales de Prueba") +
  ggtitle("Pronóstico de Ventas con NNAR") +
  xlab("Año") +
  ylab("Ventas") +
  guides(colour = guide_legend(title = "Serie"))

# Evaluar el rendimiento (ej. RMSE en el conjunto de prueba)
accuracy(forecast_nnar, test_ts)


###########################################################################

# Instalar y cargar paquetes (si aún no lo has hecho)
# install.packages("forecast")
# install.packages("ggplot2")
library(forecast)
library(ggplot2)

# --- Paso 1: Crear una serie de tiempo de ejemplo ---
# Usaremos la misma serie de tiempo de ventas mensuales
set.seed(123)
n <- 120 # 10 años de datos mensuales
tiempo <- 1:n
tendencia <- 0.5 * tiempo
estacionalidad <- 10 * sin(2 * pi * tiempo / 12) # Ciclo anual
ruido <- rnorm(n, mean = 0, sd = 5)
ventas <- 50 + tendencia + estacionalidad + ruido

# Convertir a objeto ts (time series)
ts_ventas <- ts(ventas, start = c(2015, 1), frequency = 12)
ts_ventas_df <- as.data.frame(ts_ventas)
head(ts_ventas)

# Visualizar la serie de tiempo completa
autoplot(ts_ventas) +
  ggtitle("Serie de Tiempo de Ventas Mensuales") +
  xlab("Año") +
  ylab("Ventas")


# --- Paso 2: Entrenar el modelo nnetar con TODOS los datos disponibles ---
# Para una predicción a futuro, entrenamos el modelo con la serie de tiempo COMPLETA,
# ya que queremos que aprenda de todos los datos históricos disponibles.
model_nnar_futuro <- nnetar(ts_ventas) # nnetar es bastante bueno determinando los parámetros por defecto

print(model_nnar_futuro)


# --- Paso 3: Realizar pronósticos a futuro ---
# Definimos cuántos periodos futuros queremos predecir.
# Por ejemplo, pronostiquemos los próximos 24 meses (2 años).
periodos_futuros <- 24
forecast_futuro <- forecast(model_nnar_futuro, h = periodos_futuros)

# Imprimir los valores pronosticados
print(forecast_futuro)


# --- Paso 4: Visualizar la serie de tiempo y las predicciones a futuro ---
autoplot(forecast_futuro) +
  autolayer(fitted(forecast_futuro), series = "Valores Ajustados") + # Opcional: muestra cómo el modelo ajusta los datos históricos
  ggtitle("Pronóstico de Ventas a Futuro con NNAR") +
  xlab("Año") +
  ylab("Ventas") +
  guides(colour = guide_legend(title = "Serie")) +
  theme(legend.position = "bottom")
