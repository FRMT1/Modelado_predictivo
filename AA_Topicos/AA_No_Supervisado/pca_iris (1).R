data(iris)
str(iris)

#Partition of data
set.seed(123)
id <- sample(2,nrow(iris), replace = TRUE, prob = c(0.80,0.20))
training <- iris[id == 1,]
test <- iris[id == 2,]

# Scatter plots to view milticollinarity

library(psych)
pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "green", "blue")[training$Species],
             pch = 22)

# Principal componets analysis

pc <- prcomp(training[,-5],
             center = TRUE,
             scale. = TRUE)
attributes(pc)

#Las medias de cada variable
pc$center
 #corroborando manualmente
mean(training$Sepal.Length)
# Lo mismo para la escala utilizada en la normalizacion
pc$scale
#Corroborando
sd(training$Sepal.Length)

# Visualizando la correlación entre las variables y los componentes
# Cada valor que se reporta es el coeficiente asociado a cada variable
# en la combinación lineal normalizada que conforma el componente.

print(pc)

# Veamos ahora los componentes realmente importantes

summary(pc)

# Los componentes 1 y 2 contienen más del 95% de la variabilidad total.

# Comprobando como el PCA elimina el problema de multicolinealidad

pairs.panels(pc$x,
             gap = 0,
             bg = c("red", "green","blue")[training$Species],
             pch = 22)
# Visualizando el PCA

library(ggbiplot)

bplot <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.80)
bplot +
  theme(legend.direction = "horizontal",
        legend.position = "top")
print(bplot)

# Prediccion con componentes principale
pred_PCA <- predict(pc, training)
pred_PCA <- data.frame(pred_PCA, training[5]) #Incluyendo la quinta columna
test_PCA <- predict(pc, test)
test_PCA <- data.frame(test_PCA, test[5])

# REgresion logistica multinomial para PC1 y PC2 (análisis anterior)

library(nnet)
pred_PCA$Species <- relevel(pred_PCA$Species, ref = "setosa")
model <- multinom(Species ~ PC1+PC2, data = pred_PCA)

# Matriz de confusión

p <- predict(model, pred_PCA)
tab <- table(p,pred_PCA$Species)

# Precision de la prediccion

accuracy <- sum(diag(tab))/sum(tab) * 100
accuracy
