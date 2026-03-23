library(keras3)
library(OpenImageR)

# Configura la ruta
ruta <- "~/Modelado predictivo/deep_learning/Vision_artificial/ComputerVision1"
archivos <- list.files(ruta, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)

# Dimensiones deseadas
alto <- 100
ancho <- 100

# Función para preprocesar cada imagen
procesar_imagen <- function(path) {
  img <- readImage(path)
  img_redimensionada <- resizeImage(img, width = ancho, height = alto, method = "bilinear")
  
  # Asegurar que tiene 3 canales
  if (length(dim(img_redimensionada)) == 2) {
    img_redimensionada <- array(rep(img_redimensionada, 3), dim = c(dim(img_redimensionada), 3))
  }
  
  return(img_redimensionada)
}

# Leer y procesar todas las imágenes
imagenes <- lapply(archivos, procesar_imagen)
x_data <- array(unlist(imagenes), dim = c(length(imagenes), alto, ancho, 3))

# Normalización
x_data <- x_data / 255


trainy <- c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2)


trainlabels <- to_categorical(trainy)

model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters = 16,
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(100,100,3))%>%
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                activation = 'relu')%>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu')%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_dropout(rate = 0.25)%>%
  layer_flatten()%>%
  layer_dense(units = 128, activation = 'relu')%>%
  layer_dropout(rate = 0.20)%>%
  layer_dense(units = 64, activation = 'relu')%>%
  layer_dropout(rate = 0.20)%>%
  layer_dense(units = 3, activation = 'softmax')
summary(model)

model %>%
  compile(loss = 'categorical_crossentropy',
          optimizer = optimizer_adam(),
          metrics = 'accuracy')

model%>%
  fit(x_data,
      trainlabels,
      epochs = 20,
      batch_size = 150,
      validation_split = 0.20)

model%>%
  evaluate(x_data,trainlabels)

pred_proba <- model%>%
  predict(x_data)
pred_labels <- apply(pred_proba,1, which.max)-1

library(caret)
confusion_matrix <- confusionMatrix(factor(pred_labels), factor(trainy))
confusion_matrix
