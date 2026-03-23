# Introducción
library(neuralnet)
library(RSNNS)
library(ISLR)
data = Auto
View(data)
# Análisis exploratorio de datos
plot(data$mpg~data$weight, pch = data$origin, 
     xlab = 'Peso del vehículo', ylab = 'Millas por galón (mpg)')
legend(x = 4400, y = 40, legend = c('EE.UU','Europa',
                                    'Japón'),
       pch = c(1,2,3),cex = 0.6)
par(mfrow = c(2,2))
plot(data$mpg~data$cylinders, pch = data$origin, 
     xlab = 'N° de cilindros', ylab = 'mpg')
plot(data$mpg~data$displacement, pch = data$origin,
     xlab = 'Desplazamiento', ylab = 'mpg')
plot(data$mpg~data$horsepower, pch = data$origin,
     xlab = 'Caballos de fuerza', ylab = 'mpg')
plot(data$mpg~data$acceleration, pch = data$origin,
     xlab = 'Aceleración', ylab = 'mpg')
# Normalización (Z) de los datos
normalize <- function(x){
    ((x-mean(x))/sd(x))
}
data_normalized <- as.data.frame(sapply(data[,1:6], FUN = normalize))
View(data_normalized)
#Dataframe para entrenamiento y evaluacion del modelo
muestra <- sample(1:nrow(data),round(0.70*nrow(data)))
muestra
training_data <- as.data.frame(data_normalized[muestra,])
str(training_data)
test_data <- as.data.frame(data_normalized[-muestra,])
str(test_data)
#Definición de la relación funcional
f <- as.formula(mpg~cylinders+displacement+horsepower+weight+acceleration)
f
#Modelo neuronal
model <- neuralnet(f,data = training_data,
                   hidden = 3,
                   linear.output = TRUE)
plot(model,show.weights = FALSE)
#Prediccion
par(mfrow = c(1,1))
model_results <- as.data.frame(compute(model,test_data[,2:6]))
predichos <- as.data.frame(model_results$net.result)
observados <- as.data.frame(test_data$mpg)
results <- cbind(observados,predichos)
results
plot(results$`test_data$mpg`,results$`model_results$net.result`,
     xlab = 'Observados', ylab = 'Predichos',
     main = 'Valores observados vs modelo neuronal',ylim = c(-2,2))
abline(0,1)
cor(results$`test_data$mpg`,results$`model_results$net.result`)
