#Visualización de la base de datos para una red neuronal de clasificacion
crudedata = iris
head(iris)
str(iris)
dim(iris)
#Dado que las especies son un factor, es necesario transformarla
#en una variable numerica
species <- as.numeric(iris$Specie)
crudedata <- data.frame(sepal.length = iris$Sepal.Length,
                        sepal.width = iris$Sepal.Width,
                        petal.length = iris$Petal.Length,
                        petal.width = iris$Petal.Width,
                        species)
head(crudedata)
str(crudedata)
#Visualizacion de la base de datos
plot(crudedata$petal.length,crudedata$petal.width,col =
         crudedata$species)
#Para facilitar la convergencia de la red, es conveniente
#normalizar los datos
normal <- function(x){
    (x-min(x))/(max(x)-min(x))
}
normalized_data <- sapply(crudedata[,1:4],FUN = normal)
datos <- data.frame(normalized_data,species)
head(datos,10)
#Datos de entrenamiento (70%)
id <- sample(2,nrow(datos),replace = TRUE, prob = c(0.70,0.30))
id
train_data <- datos[id==1,1:5]
#Datos de evaluacion
test_data <- datos[id==2,1:5]
#Construccion del modelo neuronal
library(neuralnet)
f = as.formula(species~sepal.length+sepal.width+petal.length+
                   petal.width)
f
model <- neuralnet(f,train_data,
                   hidden = c(3,3))
plot(model)
#Evaluacion del modelo
predicted_data <- compute(model,test_data)
results <- data.frame(actual = test_data$species,
                         predichos = round(predicted_data$net.result))    
results
table(results)
accuracy <- (10+15+14)/(10+15+14+2)
accuracy
