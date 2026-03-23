library(keras3)
mnist <- dataset_fashion_mnist()
str(mnist)

trainx <- mnist$train$x
trainy <- mnist$train$y
testx <- mnist$test$x
testy <- mnist$test$y
table(mnist$train$y, mnist$train$y)
table(mnist$test$y, mnist$test$y)

par(mfrow = c(8,8), mar = rep(0,4))
for(i in 1:64) plot(as.raster(trainx[i,,], max = 255))
par(mfrow = c(1,1))

hist(trainx[1,,])

trainx <- array_reshape(trainx, c(nrow(trainx), 28, 28, 1))
testx <- array_reshape(testx, c(nrow(testx), 28, 28, 1))
trainx <- trainx / 255
testx <- testx / 255
str(trainx)

hist(trainx[1,,,])

trainy <- to_categorical(trainy, 10)
testy <- to_categorical(testy, 10)

model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters = 16,
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(28,28,1))%>%
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu')%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_dropout(rate = 0.25)%>%
  layer_flatten()%>%
  layer_dense(units = 64, activation = 'relu')%>%
  layer_dropout(rate = 0.25)%>%
  layer_dense(units = 32, activation = 'relu')%>%
  layer_dropout(rate = 0.20)%>%
  layer_dense(units = 10, activation = 'softmax')
model

model%>%
  compile(loss = 'categorical_crossentropy',
          optimizer = optimizer_adam(),
          metrics = 'accuracy')

model %>%
  fit(trainx,
      trainy,
      epochs = 20,
      batch_size = 128,
      validation_split = 0.20)

model%>%
  evaluate(testx,testy)

pred_probs <- model%>%
  predict(testx)
pred_labels <- apply(pred_probs,1,which.max)-1


library(caret)
confusion_matrix <- confusionMatrix(factor(pred_labels), factor(mnist$test$y))
confusion_matrix
