library(h2o)
h2o.init()
data <- read.csv('/home/felix/Descargas/occupancy_data/datatraining.txt')
str(data)
occupancy_train <- read.csv(
    '/home/felix/Descargas/occupancy_data/datatraining.txt',
    stringsAsFactors = T
)
occupancy_test <- read.csv(
    '/home/felix/Descargas/occupancy_data/datatest.txt',
    stringsAsFactors = T
)
x <- c('Temperature','Humidity','Light','CO2','HumidityRatio')
y <- 'Occupancy'

occupancy_train$Occupancy <- as.factor(occupancy_train$Occupancy)
occupancy_test$Occupancy <- as.factor(occupancy_test$Occupancy)
str(occupancy_train)
str(occupancy_test)
occupancy_train_h2o <- as.h2o(x = occupancy_train,
                              destination_frame = "occupancy_train_h2o")
occupancy_test_h2o <- as.h2o(x = occupancy_test,
                             destination_frame = "occupancy_test_h2o") 
occupancy_deepmodel <- h2o.deeplearning(x = x,
                                        y = y,
                                        training_frame = occupancy_train_h2o,
                                        validation_frame = occupancy_test_h2o,
                                        activation = 'Rectifier',
                                        hidden = c(3,3),
                                        adaptive_rate = T,epochs = 100)
train_performance <- h2o.performance(occupancy_deepmodel,train = T)
train_performance@metrics$AUC
test_performance <- h2o.performance(occupancy_deepmodel,valid = T)
test_performance@metrics$AUC
predictions <- h2o.predict(occupancy_deepmodel,occupancy_test_h2o)

