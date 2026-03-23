library(caret)
ames <- AmesHousing::make_ames()

cv_pcr_model <- train(
  Sale_Price ~ .,
  data = ames,
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)

cv_pcr_model$bestTune

cv_pcr_model

ggplot(cv_pcr_model)

#################################################################################3

# La refresión de componentes principales, agrupa las variables predictoras en componentes
# t luego desarrolla la regresión (dos pasos). En ningún momento se busca mejorar la
# relación entre la variable dependiente y los predictores. La PCR solo busca disminuir
# la variabilidad y colinealidad en el espacio de las variables predictoras.