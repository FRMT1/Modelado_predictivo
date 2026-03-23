library(TSstudio)
library(forecast)
library(stats)
library(plotly)

data(USgas)
USgas


ts.plot(USgas,
        title = "US monthly natural gas consumption",
        Xtitle = "Years",
        Ytitle = "Billions of cubic feet")

USgas_split <- ts_split(USgas, sample.out = 12)        

train <- USgas_split$train
test <- USgas_split$test

USgas_model1 <- auto.arima(train)
USgas_model1

USgas_model2 <- auto.arima(
  train,
  max.order = 5,
  D = 1,
  d = 1,
  stepwise = FALSE,
  approximation = FALSE
)
USgas_model2

USgas_best_model <- arima(
  train,
  order = c(1,1,1),
  seasonal = list(order = c(2,1,1))
)
USgas_best_model

USgas_test_forecast <- forecast(USgas_best_model, h = 12)

test_forecast(
  USgas,
  forecast.obj = USgas_test_forecast,
  test = test
)

checkresiduals(USgas_best_model)

model_fc <- forecast(USgas_best_model, h = 12)
plot_forecast(model_fc,
              title = "Predicción del consumo de gas natural",
              Ytitle = "Millardos de pies cúbicos",
              Xtitle = "Años")
