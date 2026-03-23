#generacion de datos
set.seed(30)
x <- seq(from = 0, to = 30, by = 0.1)
y <- 500 + 0.25*(x-15)^3
noise <- rnorm(length(x),mean = 15,sd = 110)
noise.y <- y + noise
plot(x,noise.y,col = "darkblue",xlab = 'x',
     ylab = 'y', main = 'Datos observados')
#ajuste y evaluacion del modelo
model <- lm(noise.y~poly(x,degree = 3))
summary(model)
#prediccion de valores e intervalos de confianza
predictions <- predict(model,data.frame(x),
                               interval = 'confidence',
                               level = 0.95)
print(predictions)
#visualizacion del modelo
lines(x,predictions[,1],col = "red")
lines(x,predictions[,2],col = "black")
lines(x,predictions[,3],col = "black")
legend("bottomright",c("Observados","Predichos"),
       col = c("darkblue","red"), cex = 0.6,lwd = 0.8)



