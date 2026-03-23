require(dplyr)
library(neuralnet)
datos = (iris)
datos <- mutate(datos,Species=c(rep(1,50),rep(2,50),rep(3,50)))
head(datos)
str(datos)
n=names(datos)
f=as.formula(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)
f
nn <- neuralnet(f,
                data = datos,
                hidden = c(5,5),
                act.fct = "logistic")
plot(nn)
nn$net.result
#Evaluación del modelo
test <- data.frame(sepal.length=c(6.7,3.5,4.2),
                   sepal.width=c(2.5,3.2,3.4),
                   petal.length=c(5.8,4.2,3.6),
                   petal.width=c(1.5,2.3,1.7))
test
pred <- compute(nn,test)
pred
