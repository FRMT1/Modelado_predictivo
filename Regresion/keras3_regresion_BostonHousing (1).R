library(keras3)
library(mlbench)
library(psych)
library(dplyr)
library(magrittr)
library(neuralnet)

#Data Structure
data("BostonHousing")
str(BostonHousing)
#Preparing data
data <- BostonHousing
data$chas <- as.numeric(data$chas) #Because was a factor
str(data)
data <- data.frame(data)
#Visualizing neural network
net <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+
                   b+lstat,
                 data = data,
                 hidden = c(10,5),
                 rep = 1,
                 linear.output = FALSE)
plot(net,
     col.hidden = "darkgreen",
     col.hidden.synapse = "darkgreen",
     fill = "lightblue",
     show.weights = FALSE)
#Data partitioning
data <- as.matrix(data)
dimnames(data) <- NULL
set.seed(1234) #Repeatability
id <- sample(2, nrow(data), replace = TRUE, prob = c(.70,.30))
training <- data[id == 1, 1:13]
test <- data[id == 2, 1:13]
trainingtarget <- data[id == 1, 14]
testtarget <- data[id == 2, 14]
#Normalization (based only in training)
m <- colMeans(training)
sd <- apply(training,2, sd)
training <- scale(training, center = m, scale = sd)
test <- scale(test, center = m, scale = sd)
#Neural architecture
model <- keras_model_sequential()
model %>%
  layer_dense(units = 10, activation = 'relu', input_shape = c(13)) %>%
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dense(units = 1)
summary(model)
#Compiling the model (configuring the learning process)
model %>%
  compile(loss = 'mse',    #Because response variable is numeric
          optimizer = 'rmsprop', #Because response variable is numeric
          metrics = 'mae')   #Because response variable is numeric
model_one <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.20)

#Model evaluation (with unseen data)
model %>%
  evaluate(test, testtarget)
#Prediction of the model
pred <- model %>%
  predict(test)
cbind(pred[1:10],testtarget[1:10])
#Visualizing the prediction
par(mfrow = c(2,2))
p1 <- plot(testtarget, pred, xlab = "Actual", ylab = "Predicción",
           main = "Modelo 1")
abline(a = 0, b = 1)
#Probando modelos de aprendizaje profundo
model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape = c(13)) %>%
  layer_dropout(rate = 0.40) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.30) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 1)
summary(model)
model %>%
  compile(loss = 'mse',
          optimizer = 'rmsprop',
          metrics = 'mae')
model_two <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.20)

model %>%
  evaluate(test, testtarget)
pred <- model %>%
  predict(test)
cbind(pred[1:10], testtarget[1:10])
p2 <- plot(testtarget, pred, xlab = "Actual", ylab = "Predicción",
           main = "Modelo 2")
abline(a = 0, b = 1)
#Optimizing the model
#Log transformation of target variable
#Model 3
trainingtarget <- log(trainingtarget)
testtarget <- log(testtarget)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape = c(13)) %>%
  layer_dropout(rate = 0.40) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.30) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 1)
summary(model)
model %>%
  compile(loss = 'mse',
          optimizer = 'rmsprop',
          metrics = 'mae')
model_three <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.20)

model %>%
  evaluate(test, testtarget)
pred <- model %>%
  predict(test)
cbind(pred[1:10], testtarget[1:10])
p3 <- plot(testtarget, pred, xlab = "Actual", ylab = "Predicción",
           main = "Modelo 3 (log scale)")
abline(reg = lm(pred ~ testtarget))
#Model 4
model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape = c(13)) %>%
  layer_dropout(rate = 0.40) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate = 0.10) %>%
  layer_dense(units = 1)
summary(model)
model %>%
  compile(loss = 'mse',
          optimizer = 'rmsprop',
          metrics = 'mae')
model_four <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.20)

model %>%
  evaluate(test, testtarget)
pred <- model %>%
  predict(test)
cbind(pred[1:10], testtarget[1:10])
p4 <- plot(testtarget, pred, xlab = "Actual", ylab = "Predicción",
           main = "Modelo 4 (log scale)")
abline(reg = lm(pred ~ testtarget))

#Model two has the best performance
