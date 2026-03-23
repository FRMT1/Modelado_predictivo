library(AmesHousing)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(vip)
library(pdp)
library(ggplot2)

# Datos
mydata <- AmesHousing::make_ames() 

# Arbol de decision con RPART

tree1 <- rpart(
  data = mydata,
  formula = Sale_Price ~ .,
  method = "anova",
  control = list(cp = 0,xval = 10) # Sin parámetro de penalizacion
)
plotcp(tree1)
abline(v =20, lty = "dashed")

rpart.plot(tree1)

# El sub-arbol optimo tiene alrededor de 20 nodos

tree2 <- rpart(
  data = mydata,
  formula = Sale_Price ~ .,
  method = "anova"
)
plotcp(tree2)
abline(v = 20, lty = "dashed")

# Finalmente, el arbol optimo esta conformado por 11 nodos terminales
rpart.plot(tree2)
tree2
#Importancia de las variables
vip(tree2, num_features = 30, bar = FALSE)


# Obtención del la RMSE con CARET
set.seed(1234)
tree3 <- train(
  data = mydata,
  Sale_Price ~ .,
  method = "rpart2",
  trControl = trainControl(method = "cv")
)
ggplot(tree3)
tree3$bestTune
tree3$results
