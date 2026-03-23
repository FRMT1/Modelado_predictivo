library(keras3)

tweets <- c(
 "Este producto es excelente, lo recomiendo ampliamente. Muy satisfecho con la compra.",
 "No me gustó para nada. Demasiado caro y no cumple lo prometido. Una decepción total.",
 "Es un producto decente, hace su trabajo, pero podría mejorar en algunos aspectos.",
 "Fantástico! Superó mis expectativas en todos los sentidos. Volvería a comprarlo sin duda.",
 "Pésima calidad. Se rompió al poco tiempo de usarlo. No lo compren, es una estafa."
)


# Tokenization

vectorize_layer <- layer_text_vectorization(
  max_tokens = 100,         # SElecciona las 20 palabras más importantes
  standardize = "lower_and_strip_punctuation",
  output_mode = "int",
  ngrams = NULL,
  output_sequence_length = 20
)

vectorize_layer %>% adapt(tweets)

vectorize_layer$get_vocabulary()
vocabulary_size <- length(vectorize_layer$get_vocabulary())
vocabulary_size

x_train <- vectorize_layer(tweets)   # Coversión a números
x_train 

sentimientos <- c(1,0,1,1,0)
y_train <- array(sentimientos)

model <- keras_model_sequential() %>%
  vectorize_layer() %>%
  layer_embedding(
    input_dim = vocabulary_size,
    output_dim = 120
  ) %>%
  layer_lstm(units = 30) %>%
  layer_dense(units = 1, activation = "sigmoid")
 
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

histoty_training <- model %>% fit(
  x = tweets,    # Ojo, se pasan los tweets originales
  y = y_train,
  epochs = 20,
  validation_split = 0.0,
  batch_size = 1,
)

results <- model %>%
  evaluate(tweets, y_train)
results

nuevos_tweets <- c(
  "Este producto es simplemente maravilloso, estoy fascinado.",
  "Aceptable experiencia, no perdí mi dinero.",
  "Funciona bien, cumple su función, pero nada excepcional.",
  "Me encanta, superó mis expectativas, muy recomendable.",
  "No lo recomiendo en absoluto, una pérdida de dinero."
)

predictions <- predict(model, nuevos_tweets)
predicted <- as.numeric(predictions)
predicted

predicted_classes <- as.numeric(ifelse(predictions >= .5, 1, 0))
predicted_classes

df <- data.frame(
  Texto = nuevos_tweets,
  Probabilidad = predicted,
  Sentimiento = predicted_classes
)

df
