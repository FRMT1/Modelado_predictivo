library(h2o)
h2o.init()
mydata <- iris
mydata <- as.h2o(mydata)
h2o.colnames(mydata)
h2o.str(mydata)
mydata[,5] <- h2o.asnumeric(mydata[,5])
h2o.str(mydata)

mydata[,1:4] <- h2o.scale(mydata[,1:4], center = T, scale = T)
h2o.head(mydata)

mydata_split <- h2o.splitFrame(mydata, 
                               ratios = 0.70,
                               seed = 1234)

train_h2o <- mydata_split[[1]]
test_h2o <- mydata_split[[2]] 

model <- h2o.deeplearning(
  x = 1:4,
  y = 5,
  training_frame = train_h2o,
  validation_frame = test_h2o,
  activation = "tanh",
  hidden = c(3,3),
  epochs = 200
)

h2o.performance(model = model)
predictions <- h2o.predict(model, mydata[,1:4])

actual <- as.vector(mydata$Species)
predichos <- as.vector(round(predictions))
results <- data.frame(actual,predichos)
table(results) 

Accuracy <- ((50+49+50)/150)*100
Accuracy
