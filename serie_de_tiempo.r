#Bibliotecas a utilizar
library(zoo)
library(quantmod)
library(neuralnet)
library(Metrics)
#Base de datos
data("ecoli",package = "tscount")
class(ecoli)
View(ecoli)
str(ecoli)

#Almacenamiento de la base de datos y conversión en objeto ts
data <- as.numeric(unlist(ecoli[3]))
data <- ts(matrix(data),
           start = c(2001,1),
           end = c(2013,20),
           frequency = 52)
head(data)
#Visualización
plot(data,xlab="Fecha",ylab="N° de casos",col="darkblue")
#Buscando correlaciones parciales
pacf(data)
#Creacion de la matriz con las 4 ultimas fechas acorde a pacf
data <- as.zoo(data)
x1 <- Lag(data, k = 1)
x2 <- Lag(data, k = 2)
x3 <- Lag(data, k = 3)
x4 <- Lag(data, k = 4)
x <- cbind(x1,x2,x3,x4,data)
tail(x)
head(x)
#Eliminación de los NA
x <- x[-(1:4),] 
head(x)
#Normalización de los datos
x <- as.data.frame(x)
head(x)
data_min <- apply(x,2,min)
data_min
data_max <- apply(x,2,max)
data_max
data_norm <- function(x){
    (x-data_min)/(data_max-data_min)
}
x_norm <- data_norm(x)
summary(x_norm[,1:2])
summary(x_norm[,3:4])
summary(x_norm[,5])
head(x_norm)
# Definiendo datos de entrenamiento y evaluación
nrow(x_norm)
n_train <- 600

data_train <- x_norm[1:n_train,]
data_test <- x_norm[(n_train+1):nrow(x),]
f <- as.formula(data~Lag.1+Lag.2+Lag.3+Lag.4)

#Desarrollo del modelo neuronal con Neuralnet
set.seed(1234)
model <- neuralnet(f,
                   data = data_train,
                   hidden = c(3,3),
                   act.fct = 'tanh',
                   learningrate = 0.6)
plot(model)
#Predicción y evaluación con MSE
predict_model_train <- compute(model,data_train[,1:4])
pm_train <- predict_model_train$net.result
dtrain <- data_train[,5]
round(rmse(pm_train,dtrain),3)

predict_model_test <- compute(model,data_test[,1:4])
pm_test <- predict_model_test$net.result
dtest <- data_test[,5]
round(rmse(pm_test,dtest),3)

# "Desnormalizando los datos"
unscale_data <- function(dtest,min_x,max_x){
    (dtest*(max_x-min_x))+min_x
}
y_actual_test <- unscale_data(dtest,data_min,data_max)
y_actual_train <- unscale_data(dtrain,data_min,data_max)
pred_actual_test <- unscale_data(pm_test,data_min,data_max)
pred_actual_train <- unscale_data(pm_train,data_min,data_max)

#Colocando en formato de serie de tiempo
y_actual <- ts(matrix(y_actual_test),
               end = c(2013,20),
               frequency = 52)
pred_actual <- ts(matrix(pred_actual_test),
                  end = c(2013,20),
                  frequency = 52)
#Visualización con los datos de evaluación
ts.plot(y_actual,pred_actual,
        gpars = list(xlab='Años',ylab='N° de casos',
                     main='Datos de evaluación',
                     col=c('red','blue')))
#Visualización con los datos de entrenamiento
y_actual <- ts(matrix(y_actual_train),
               end = c(2013,20),
               frequency = 52)
pred_actual <- ts(matrix(pred_actual_train),
                  end = c(2013,20),
                  frequency = 52)
ts.plot(y_actual,pred_actual,
        gpars = list(xlab='Años',ylab='N° de casos',
                     main='Datos de entrenamiento',
                     col=c('red','blue')))


