library(caret)
ames <- AmesHousing::make_ames()

cv_pls_model <- train(
  Sale_Price ~ .,
  data = ames,
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)

cv_pls_model
summary(cv_pls_model)

cv_pls_model$bestTune

ggplot(cv_pls_model)

vip::vip(cv_pls_model)


##########################################################################################3

# La regresión por mínimos cuadrados parciales, reduce el número de variables predictoras
# por lo que es considerada una técnica de reducción supervisada de la dimensionalidad
# y a diferencia de la PCR, toma en cuenta la mejor relación con la variable respuesta.

#########################################################################################
