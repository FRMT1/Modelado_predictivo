# 1. Crear datos de ejemplo (sustituye esto con tus datos reales)
set.seed(42) # Para reproducibilidad

# Tiempo: de 0 a 100
X <- seq(0, 100, length.out = 200)

# Datos experimentales: una curva de ejemplo (p. ej., una función gaussiana + ruido)
Y <- 10 * exp(-(X - 50)^2 / (2 * 15^2)) + rnorm(200, mean = 0, sd = 0.5)

# Asegúrate de que Y sea no negativo (es una "curva" en un contexto de área)
Y[Y < 0] <- 0
plot(Y~X)

  N = 100000
  # 1. Definir el rectángulo de acotación
  x_min <- min(X)
  x_max <- max(X)
  y_min <- 0  # Asumimos que el área es sobre el eje X (Y >= 0)
  y_max <- max(Y)
  
  # Área total del rectángulo de acotación
  area_rectangulo <- (x_max - x_min) * (y_max - y_min)
  
  # 2. Generar puntos aleatorios dentro del rectángulo
  # Generar N puntos aleatorios para X
  rand_x <- runif(N, min = x_min, max = x_max)
  
  # Generar N puntos aleatorios para Y
  rand_y <- runif(N, min = y_min, max = y_max)
  
  # 3. Determinar qué puntos caen bajo la curva
  # Para datos discretos, necesitamos una función de interpolación.
  # Usaremos 'approxfun' para crear una función que interpole linealmente
  # entre los puntos de tus datos.
  f_interpolada_spline <- splinefun(X, Y, method = "natural")
  
  # Valor de la curva (f_interpolada) en las X aleatorias
  curve_y_at_rand_x <- f_interpolada_spline(rand_x)
  
  # Contar los puntos que caen bajo la curva (rand_y < f(rand_x))
  puntos_bajo_curva <- sum(rand_y < curve_y_at_rand_x)
  
  # 4. Calcular el área estimada
  proporcion_exito <- puntos_bajo_curva / N
  area_mc <- proporcion_exito * area_rectangulo

area_mc



# Instalar si no lo tienes: install.packages("pracma")
library(pracma)

# Área calculada por el método del trapecio (generalmente más preciso para datos discretos)
area_trapz <- trapz(X, Y)

print(paste("Área estimada por el Método del Trapecio:", round(area_trapz, 3)))

# Comparación del error
error_relativo <- abs(area_mc - area_trapz) / area_trapz * 100
print(paste("Error relativo (comparado con Trapecio):", round(error_relativo, 2), "%"))
