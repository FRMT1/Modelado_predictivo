library(caret)
library(earth)
library(dplyr)
library(vip)
library(MASS)

# Ejemplo 1

env <- environmental
View(env)

mars_model1 <- earth(
  ozone ~ .,
  data = env
)
mars_model1
mars_model1$coefficients
plot(mars_model1, which = 1)

grid <- expand.grid(
  degree = 1:3,
  nprune = seq(2, 20, length.out = 10)
)
head(grid)

set.seed(123)
mars_model_optimo <- train(
  ozone ~ .,
  data = env,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = grid
)

mars_model_optimo$bestTune
mars_model_optimo$results
mars_model_optimo

ggplot(mars_model_optimo)

g1 <- vip(mars_model_optimo, num_features = 6, bar = FALSE, value = "gcv")+
  ggtitle("GCV")
g1

################################################################################3

# Ejemplo 2

ames <- AmesHousing::make_ames()
str(ames)

mars_model <- earth(
  Sale_Price ~ .,
  data = ames
)
mars_model
mars_model$coefficients
plot(mars_model, which = 1)

mars_model2 <- earth(
  Sale_Price ~ .,
  data = ames,
  degree = 2
)
mars_model2
mars_model2$coefficients
plot(mars_model2, which = 1)
df1 <- data.frame(Observados = ames$Sale_Price, Predichos = mars_model2$fitted.values)
head(df1,20)
colnames(df1) <- c("Observados", "Predichos")
head(df1,7)

ggplot(df1, aes(x = Observados, y = Predichos))+
  geom_point(size = 1, alpha = 0.4)+
  geom_smooth(method = "lm", se = FALSE)+
  labs(
    title = "Precio de venta",
    subtitle = "Modelo mars 2",
    x = "Precio observado (USD)",
    y = "Precio predicho (USD)"
  )

g2 <- vip(mars_model2, num_features = 26)+
  ggtitle("Variables de importancia")
g2

################################################################################3

# Ejemplo 3

data <- MASS::mcycle
View(data)
str(data)

plot(accel ~ times, data = data, pch = 21)

mars_model <- earth(
  accel ~ times,
  data = data,
)
mars_model
plot(mars_model, which = 1)
mars_model$coefficients
plot(mars_model)

plot(accel ~ times, data = data, pch = 21)
lines(data$times, predict(mars_model), col = "darkblue")

#################### Ajustando el modelo

mars_model2 <- earth(
  accel ~ times,
  data = data,
  minspan = 1,
  thresh = 0
)

mars_model2$coefficients

plot(accel ~ times, data = data) 
lines(data$times, predict(mars_model2, col = "red"))
summary(mars_model2)

##### New data for mars_model2

new_data <- data.frame(times = c(28,29,30,31,32,33,34,35))
new_data 

pred <- predict(mars_model2, newdata = new_data)
pred
