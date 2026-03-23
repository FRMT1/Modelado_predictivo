library(astsa)          # Bibliotecas requeridas
library(quantmod)
library(zoo)
data("chicken", package = "astsa")
plot(chicken,               # Visualización de la serie de tiempo
     xlab = "Año",
     ylab = "Centavos de dolar (EEUU)",
     col = "darkblue")
data <- chicken
data <- as.zoo(data)   # Creación de la autoregresión
x1 <- Lag(data, k = 1)
x2 <- Lag(data, k = 2)
x3 <- Lag(data, k = 3)
x4 <- Lag(data, k = 4)

x <- cbind(x1,x2,x3,x4,data) # Integración en un dataframe
head(round(x,2))
x <- x[-(1:4),]
head(round(x,2))

x <- data.matrix(x)  # Normalización de los datos
norm_data <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
min_data <- min(data)
max_data <- max(data)
x <- norm_data(x)

min(x) # Comprobación
max(x)

x1 <- as.matrix(x[,1])    #Formato de matriz para procesar los datos
x2 <- as.matrix(x[,2])
x3 <- as.matrix(x[,3])
x4 <- as.matrix(x[,4])
y <- as.matrix(x[,5])

n_train <- 170         # Separación de los datos de entrenamiento
y_train <- as.matrix(y[1:n_train])
x1_train <- as.matrix(t(x1[1:n_train,]))  # Pendiente con la traspuesta
x2_train <- as.matrix(t(x2[1:n_train,]))
x3_train <- as.matrix(t(x3[1:n_train,]))
x4_train <- as.matrix(t(x4[1:n_train,]))

nrow(x)           # Comprobación
nrow(y_train)
ncol(x1_train)
ncol(x2_train)
ncol(x3_train)
ncol(x4_train)

library(rnn)  # Biblioteca: redes neuronales recurrectes

x_train <- array(c(x1_train,   # Arreglo de 3 dimensiones
                   x2_train,   # Nªmuestra, pasos de tiempo, Nª independientes
                   x3_train,
                   x4_train),
                 dim = c(dim(x1_train),4))
dim(x_train)        # Comprobación


set.seed(2022)        # Modelo 1
model1 <- trainr(Y = t(y_train),     # Ojo con traspuesta
                 X = x_train,
                 learningrate = 0.05,
                 hidden_dim = 3,
                 numepochs = 400,
                 sigmoid = "logistic",
                 network_type = "rnn") 

error_model1 <- t(model1$error) # Disminución de error en entrenamiento
plot(error_model1)

pred_model1 <- t(predictr(model1,x_train))  # Predicción
round(cor(y_train, pred_model1),3) # Correlacón con los datos de entrenamiento

# Mejoramiento del modelo

set.seed(2024)       # Modelo 1
model2 <- trainr(Y = t(y_train),     # Ojo con traspuesta
                 X = x_train,
                 learningrate = 0.1,
                 hidden_dim = 4,
                 numepochs = 400,
                 momentum = 0.8,
                 sigmoid = "logistic",
                 network_type = "rnn") 

error_model2 <- t(model2$error) # Disminución de error en entrenamiento
plot(error_model2)

pred_model2 <- t(predictr(model2,x_train))  # Predicción
round(cor(y_train, pred_model2),3) # Correlacón con los datos de entrenamiento

par(mfrow = c(1,2))
plot(y_train, pred_model1, ylab = "Modelo 1", col = "darkblue")
abline(reg = lm(pred_model1 ~ y_train), col = "red") 
plot(y_train, pred_model2, ylab = "Modelo 2", col = "darkblue")
abline(reg = lm(pred_model2 ~ y_train), col = "red") 

# Validación de los modelos

x1_test <- as.matrix(t(x1[(n_train+1):nrow(x1),]))  # Pendiente con la traspuesta
x2_test <- as.matrix(t(x2[(n_train+1):nrow(x2),]))
x3_test <- as.matrix(t(x3[(n_train+1):nrow(x3),]))
x4_test <- as.matrix(t(x4[(n_train+1):nrow(x4),]))
y_test <- as.matrix(y[(n_train+1):nrow(x4)])

x_test <- array(c(x1_test,   # Arreglo de 3 dimensiones
                   x2_test,   # Nªmuestra, pasos de tiempo, Nª independientes
                   x3_test,
                   x4_test),
                 dim = c(dim(x1_test),4))
dim(x_test) 

# Predicción de los datos de validación

pred_model1_test <- t(predictr(model1,x_test)) 
pred_model2_test <- t(predictr(model2,x_test)) 

# Desescalando los datos

unscale_data <- function(x, max_x, min_x){           # Función para desescalar
  x * (max_x - min_x) + min_x
}

pred_model1_test_unscaled <- unscale_data(pred_model1_test,
                                          max_data,
                                          min_data)     # Para Modelo 1
pred_model1_test_unscaled <- ts(matrix(pred_model1_test_unscaled),
                           end = c(2016,7),
                           frequency = 12)

pred_model2_test_unscaled <- unscale_data(pred_model2_test,
                                          max_data,
                                          min_data)   # Para Modelo 2
pred_model2_test_unscaled <- ts(matrix(pred_model2_test_unscaled),
                           end = c(2016,7),
                           frequency = 12)

y_actual <- unscale_data(y_test,
                         max_data,
                         min_data)         # Para los datos observados
y_actual <- ts(matrix(y_actual),
               end = c(2016,7),
               frequency = 12)

results <- cbind(round(y_actual, 3),
                 round(pred_model1_test_unscaled, 3),
                 round(pred_model2_test_unscaled, 3))

colnames(results) <- c("Observados", "Modelo 1", "Modelo 2")

results

# PRONÓSTICO DE 6 MESES

start = n_train
end = 7
k = -1

for(i in start:((start+end))){
  k = k+1
  
  # Preparando los datos de entrenamiento
  
  y_train_step<- as.matrix(y[1:(n_train + k)])
  x1_train_step <- as.matrix(t(x1[1:(n_train + k),]))  # Pendiente con la traspuesta
  x2_train_step <- as.matrix(t(x2[1:(n_train + k),]))
  x3_train_step <- as.matrix(t(x3[1:(n_train + k),]))
  x4_train_step <- as.matrix(t(x4[1:(n_train + k),]))
  
  x_train_step <- array(c(x1_train_step,
                          x2_train_step,
                          x3_train_step,
                          x4_train_step),
                        dim = c(dim(x1_train_step), 4))
  
  # Modelo neuronal
  
  set.seed(2300)
  model_step <- trainr(Y = t(y_train_step),     # Ojo con traspuesta
                   X = x_train_step,
                   learningrate = 0.05,
                   hidden_dim = 3,
                   numepochs = 400,
                   momentum = 0.8,
                   sigmoid = "logistic",
                   network_type = "rnn") 

  
  # Preparando los datos de validación
  
  y_test_step<- as.matrix(y[(n_train + k + 1)])
  x1_test_step <- as.matrix(t(x1[(n_train + k),]))  # Pendiente con la traspuesta
  x2_test_step <- as.matrix(t(x2[(n_train + k),]))
  x3_test_step <- as.matrix(t(x3[(n_train + k),]))
  x4_test_step <- as.matrix(t(x4[(n_train + k),]))
  
  x_test_step <- array(c(x1_test_step,
                          x2_test_step,
                          x3_test_step,
                          x4_test_step),
                        dim = c(dim(x1_test_step), 4))
  
  # Pronóstico
  
  pred_model_step <- t(predictr(model_step, x_test_step))
  
  if(i == start){forecast <- as.numeric(pred_model_step)}
  if(i > start){forecast <- cbind(forecast, as.numeric(pred_model_step))}
} 

# Desescalando el pronóstico
  
  pred_model_step_unscaled <- unscale_data(forecast,
                                            max_data,
                                            min_data)     # Para Modelo 1
  pred_model_step_unscaled <- ts(matrix(pred_model_step_unscaled),
                                  end = c(2016,8),
                                  frequency = 12) 

# Publicación de resultados
  
results <- cbind(round(y_actual, 3),
                 round(pred_model1_test_unscaled, 3),
                 round(pred_model2_test_unscaled, 3),
                 round(pred_model_step_unscaled, 3))

colnames(results) <- c("Observados", "Modelo 1", "Modelo 2",
                       "Pronóstico (7 m)")

results

# Visualización de los datos observados y los modelos neuronales
#durante el entrenamiento

y_actual <- unscale_data(y_train,
                         max_data,
                         min_data)         # Para los datos observados
y_actual <- ts(matrix(y_actual),
               end = c(2016,1),
               frequency = 12)

pred_model1_unscaled <- unscale_data(pred_model1,
                                          max_data,
                                          min_data)     # Para Modelo 1
pred_model1_unscaled <- ts(matrix(pred_model1_unscaled),
                                end = c(2016,1),
                                frequency = 12)

pred_model2_unscaled <- unscale_data(pred_model2,
                                          max_data,
                                          min_data)   # Para Modelo 2
pred_model2_unscaled <- ts(matrix(pred_model2_unscaled),
                                end = c(2016,1),
                                frequency = 12)


ts.plot(y_actual,pred_model1_unscaled,pred_model2_unscaled,
        gpars = list(xlab = "Años",
                     ylab = "$ cent./Lb de pollo entero",
                     main = "Precio del pollo entero crudo (2001-2016)",
                     sub = "Predicción por dos modelos neuronales recurrentes",
                     col = c("darkblue", "red", "darkgreen"))) 
legend(x = "bottomright",
       legend = c("Observados","Red neuronal 1","Red neuronal 2"),
       fill = c("darkblue","red","darkgreen"))   
