library(psych); library(ggbiplot)

data(iris)
str(iris)
summary(iris)

set.seed(111)
id <- sample(2, nrow(iris), replace = T, prob = c(0.80,0.20))
training <- iris[id == 1,]
testing <- iris[id == 2,]

pairs.panels(training[,-5],    # Esto demuestra la multicilinealidad
             gap = 0,
             bg = c("red","blue","darkgreen")[training$Species],
             pch = 21)

pc <- prcomp(training[,-5],
             center = TRUE,
             scale = TRUE)

attributes(pc)

pc$center
mean(training$Sepal.Length)  # Estos son los centros

pc$scale
sd(training$Sepal.Length)  # Desviación estándar de los datos normalizados

print(pc)    # Muy importante
summary(pc) # Muy importante

pairs.panels(pc$x,         # Demuestra como se eliminó la multicolinealidad
             gap = 0,
             bg = c("red", "blue", "darkgreen")[training$Species],
             pch = 21)


# ggbiplot

g <- ggbiplot(pc, 
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = TRUE,
              ellipse.prob = 0.70,
              circle = TRUE)
g <- g + scale_color_discrete()
g <- g + theme(legend.direction = "horizontal",
               legend.position = "top")
print(g)


# Prediccion del espacio de predictores con PC
# Sustituye las variables originales por los componentes principales

trg <- predict(pc, training)
trg <- data.frame(trg, training[5])
tst <- predict(pc, testing)
tst <- data.frame(tst, testing$Species)

# Uso de la dimension reducida (PC1 y PC2), para un modelo de ML

library(party)
library(caret)

model <- ctree(Species ~ PC1+PC2, data = trg)

CM <- confusionMatrix(
  data = factor(predict(model, tst)),
  reference = factor(tst$testing.Species)
)
CM

# Este ejemplo es para un propósito ilustrativo, ya que NO se requiere disminuir
# la dimensión del espacio predictor cuando solo se tienen 4 predictores
