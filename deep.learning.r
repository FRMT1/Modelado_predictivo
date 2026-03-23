#Bibliotecas
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

#Base de datos
data("BostonHousing")
data <- BostonHousing
str(data)
?BostonHousing

#Todas las variables como numericas
data %<>% mutate_if(is.factor,as.numeric)
str(data)

#Normalizacion
norma <- function(x){
    (x-mean(x))/(sd(x))
}
data <- sapply(data,FUN = norma)
head(data)

#Datos de entrenamiento y evaluacion
id <- sample(2,nrow(data),replace = T, prob = c(.7,.3))
training <- data[id==1,1:14]
test <- data[id==2,1:14]

#Relacion funcional
f <- as.formula(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+
                    tax+ptratio+b+lstat)
f

#Definicion del modelo
model <- neuralnet(f,
                   data = training,
                   hidden = c(5,5),
                   learningrate = 0.002)
plot(model,
     show.weights = F,
     fill="lightblue")

#Prediccion
eval <- compute(model,data[,1:13])
pred <- eval$net.result*sd(data[,14])+mean(data[,14]) 
final_output <- cbind(data[,14],as.data.frame(pred))
head(final_output)
colnames(final_output) <- c("observados","predichos")
head(final_output)
x <- final_output$observados
y <- final_output$predichos
plot(y~x,xlab = "observados",ylab = "predichos")
abline(z <- lm(y~x))
points(x,y,col = "blue")
summary(z)
