#Visualización de la base de datos para una red neuronal de clasificacion
crudedata = iris
head(iris)
str(iris)
dim(iris)
#Dado que las especies son un factor, es necesario transformarla
#en una variable numerica
species <- as.factor(iris$Specie)
data <- data.frame(sepal.length = iris$Sepal.Length,
                        sepal.width = iris$Sepal.Width,
                        petal.length = iris$Petal.Length,
                        petal.width = iris$Petal.Width,
                        species)
head(data)
str(data)

#Datos de entrenamiento (70%)
set.seed(123)
id <- sample(2,nrow(data),replace = TRUE, prob = c(0.70,0.30))
id
train_data <- data[id==1,]
#Datos de evaluacion
test_data <- data[id==2,]
#Construccion del modelo neuronal
library(party)
f = as.formula(species~sepal.length+sepal.width+petal.length+
                   petal.width)
f
model <- ctree(f, data = train_data,
               controls = ctree_control(mincriterion = 0.95,
                                        minsplit = 30))
plot(model)
pred <- predict(model,
                newdata = test_data,type = "prob")
pred
r <-table(predict(model,test_data),test_data$species)
accu <- (sum(diag(r))/sum(r))
accu
r
