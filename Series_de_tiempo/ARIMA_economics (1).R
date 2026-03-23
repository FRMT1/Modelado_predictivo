library(ggplot2)
library(TSstudio)
library(forecast)
library(stats)
library(plotly)
library(dplyr)

data("economics")
str(economics)
head(economics,20)

df <- ts(economics$unemploy, start = 1967, end = 2023, frequency = 12)

df_split <- ts_split(df, sample.out = 24)
train <- df_split$train
test <- df_split$test

ts.plot(df,
        title = 'Tasa de desempleo (1967-2015)',
        Xtitle = 'Años',
        Ytitle = 'Desempleo')

economics_model <- auto.arima(train)
economics_model

economics_test_forecast <- forecast(economics_model, h = 24)

test_forecast(
  df,
  forecast.obj = economics_test_forecast,
  test = test
)


