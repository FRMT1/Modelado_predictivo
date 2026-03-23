#Llamado de la biblioteca RSNNS y neuralnet
library(neuralnet)
library(RSNNS)
#Base de datos Iris
data(iris)
head(iris,n=20)
#Aleatorizacion de iris
iris=iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]
head(iris,n=20)
#Variables independientes y dependientes
iris_ind = iris[,1:4]
iris_target = decodeClassLabels(iris[,5])
#Definicion de los dataframe para entrenamiento y para evaluacion
iris = splitForTrainingAndTest(iris_ind,iris_target,ratio = 0.15)
iris
iris = normTrainingAndTestSet(iris)
#Especificando el modelo neuronal
model = mlp(iris$inputsTrain,
            iris$targetsTrain,
            size = 5,
            maxit = 50,
            inputsTest = iris$inputsTest,
            targetsTest = iris$targetsTest)
#Grafico de caida del error
plotIterativeError(model)
#Predicciones con el modelo neuronal
predictions = predict(model,iris$inputsTest)
predictions
confusionMatrix(iris$targetsTrain,predict(model))
confusionMatrix(iris$targetsTest,predict(model))


