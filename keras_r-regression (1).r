library(keras)
library(tensorflow)
library(dplyr)
library(mlbench)
library(neuralnet)
data("BostonHousing")
View(BostonHousing)
data <- BostonHousing
str(data)
data <- data%>%mutate_if(is.factor,as.numeric)
#Visualizacion del modelo
mod <- neuralnet(data = data,
                 formula = medv~crim+zn+indus+chas+nox+rm+age+dis+
                   rad+tax+ptratio+b+lstat,
                 hidden =c(7,3))
plot(mod, show.weights = F, fill = "lightgreen" )
# Transformación a matriz
data <- as.matrix(data)
#Particion de los datos
set.seed(1234)
index <- sample(2,nrow(data),replace = T, prob = c(0.70,0.30))
training <- data[index == 1,1:13]
test <- data[index ==2,1:13]
trainingtarget <- data[index == 1,14]
testtarget <- data[index == 2,14]
#Normalizacion
m <- colMeans(training)
s <- apply(training,2,sd)
training <- scale(training,center = m, scale = s)
test <- scale(test,center = m, scale = s)
#Arquitectura neuronal
model <- keras_model_sequential()
model %>% #capas de neuronas
  layer_dense(units = 50,activation = "relu",input_shape = c(13))%>%
  layer_dropout(0.30) %>%
  layer_dense(units = 20, activation = "relu") %>%
  layer_dropout(0.30) %>%
  layer_dense(units = 1)
model %>% #Compilacion
  compile(loss = "mse",
          optimizer = optimizer_rmsprop(0.001),
          metrics = "mae"
    )
model %>%
  fit(
    training,
    trainingtarget,
    epochs = 150,
    batch_size = 25,
    validation_split = 0.20
  )
#Evaluacion del modelo
model %>%
  evaluate(test,testtarget)
pred <- model %>%
  predict(test)
mean(abs(testtarget-pred))
mean((testtarget-pred)^2)
library(ggplot2)
testtarget <- as.vector(testtarget)
pred <- as.vector(pred)
p <- data.frame(testtarget,pred)
ggplot(p, aes(testtarget,pred))+
  geom_point(size = 3, color = "Blue")+
  geom_smooth(se = F, method = "lm")+
  theme_classic()+
  labs(x = "Observados", y = "Predichos",
       title = "Modelo neuronal")

  