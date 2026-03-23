#Area bajo la curva (datos experimentales)
#Metodo 1 (Simulación Monte Carlo)

x <- seq(0, 100, length.out = 200)
y <- 10*exp(-(x-50)^2/15^2)
plot(y~x)

x_min <- min(x)
x_max <- max(x)
y_min <- min(y)
y_max <- max(y)

area_cuadrado <- (y_max - y_min) * (x_max - x_min)
f_spline <- splinefun(x,y, method = "natural")

N = 100000
set.seed(1234)
rand_x <- runif(N, x_min, x_max)
rand_y <- runif(N, y_min, y_max)
curva_interpolada_en_randx <- f_spline(rand_x)

puntos_bajo_curva <- sum(rand_y <= curva_interpolada_en_randx)
proporcion_exito <- puntos_bajo_curva/N
area_mc <- proporcion_exito * area_cuadrado

# Metodo 2 (Trapecio)
library(pracma)

area_trapz <- trapz(x,y)

# Comparacion metodos 1 y 2

error_relativo <- abs(area_mc - area_trapz)/area_trapz * 100

print(paste("Área con el método Monte Carlo:", round(area_mc,3)))
print(paste("Área con el método del trapecio:", round(area_trapz,3)))
print(paste("Error relativo:", round(error_relativo,2), "%"))



