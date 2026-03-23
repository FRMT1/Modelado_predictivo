library(randomForest); library(caret)

data <- read.csv("~/Modelado_predictivo/machine_learning/Random_forest/CTG.csv",
                 header = T)
str(data)
data$NSP <- as.factor(data$NSP)
data$Nzeros <- as.factor(data$Nzeros)
sum(is.na(data)) 
table(data$NSP)

set.seed(123)
id <- sample(2, nrow(data), replace = T, prob = c(0.70,0.30))
training <- data[id == 1,]
testing <- data[id == 2,]

model_rf1 <- randomForest(NSP ~ ., data = training, ntree = 600, importance = T,
                          proximity = T)

pred <- predict(model_rf1, testing)

cm1 <- confusionMatrix(
  data = factor(pred),
  reference = factor(testing$NSP),
  positive = "3"
)
cm1

model_rf1$importance
varImpPlot(model_rf1, sort = T)

# Aun sin corregir el desbalance de clases, el modelo de bosques aleatorios da mejores
# resultados que el modelo de redes neuronales

# Segun las variables de importancia, por la magnitud del descenso del indice de Gini
# el modelo puede ser mejorado usando aproximadamente 5 variables
