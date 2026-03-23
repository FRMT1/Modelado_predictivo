# Ejemeplo de analisis de correlacion desde cero
tde <- c(9.95,24.45,31.75,35,25.02,16.86,14.38,9.60,24.35,27.50,
         17.08,37,41.95,11.66,21.65,17.89,69,10.30,34.93,46.59,
         44.88,54.12,56.63,22.13,21.15)
ndl <- c(2,8,11,10,8,4,2,2,9,8,4,11,12,2,4,4,20,1,10,15,15,16,
         17,6,5)
data <- data.frame(ndl,tde)
data
# Siendo x = ndl, y = tde
# Sumatoria de x y sumatoria de x_1^2
suma_x <- sum(data$ndl)
suma_x2 <- sum((data$ndl)^2)
# Sumatoria de y y sumatoria de y_i^2
suma_y <- sum(data$tde)
suma_y2 <- sum((data$tde)^2)
# Sumatoria x*y
suma_xy <- sum(data$ndl*data$tde)
# Calculo de Sxx
Sxx <- suma_x2-(suma_x^2/length(data$ndl))
print(Sxx)
# Calculo de Syy
Syy <- suma_y2 - (suma_y^2/length(data$tde))
print(Syy)
# Calculo de Sxy
Sxy <- suma_xy - (suma_x*suma_y)/length(data$tde)
print(Sxy)
# Calculo del coeficiente de correlacion
r <- (Sxy/(Sxx*Syy)^0.5)
print(r)
# Calculo de la estadística de prueba to
to <- (r*sqrt(length(data$ndl)-2))/(sqrt(1-r^2))
print(to)
ttab <- (qt(0.975,length(data$tde)-2))
print(ttab)
# Dado que to es mayor que ttab, se rechaza la hipotesis nula
# concluyendose que el coeficiente de correlacion es 
# significativamente diferente de cero

# Utilizando las herramientas de R
library(agricolae)
correlation(ndl,tde,method = 'pearson')
