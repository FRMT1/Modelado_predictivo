#Bibliotecas de R
library(nnet)
library(NeuralNetTools)
#Datos de entrenamiento para red neuronal de clasificación
datos=read.csv("/home/profesor/Redes Neuronales con R/red de clasificacion.csv",
               sep = ",", header = TRUE)
datos
attach(datos)
names(datos)
#Entrenamiento de la red
model=nnet(recomienda~servicio+ambiente+alimentos,
           data = datos,
           size = 5,
           rang = 0.1,
           decay =5e-2,
           maxit =5000)
print(model)
plotnet(model)
garson(model)
