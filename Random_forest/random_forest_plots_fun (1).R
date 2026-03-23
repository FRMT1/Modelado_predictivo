# Cargar librería
library(randomForest)

# Datos de ejemplo (puedes reemplazarlos por los tuyos)
set.seed(123)
df <- data.frame(
  x = sort(runif(100, 0, 10)), 
  y = sin(sort(runif(100, 0, 10))) + rnorm(100, sd = 0.3)
)

# Entrenar el modelo
modelo_rf <- randomForest(y ~ x, data = df)

# Crear un grid de valores de x para predecir y graficar suavemente
x_grid <- data.frame(x = seq(min(df$x), max(df$x), length.out = 200))
predicciones <- predict(modelo_rf, newdata = x_grid)

# Graficar
plot(df$x, df$y, pch = 16, col = "darkgray", 
     xlab = "x", ylab = "y", main = "Random Forest: Observado vs Predicho")

# Línea de predicción
lines(x_grid$x, predicciones, col = "blue", lwd = 2)

###########################################################################3


