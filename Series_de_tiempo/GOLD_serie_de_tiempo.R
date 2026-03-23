library(zoo)
library(forecast)

data(gold)
gold

gold <- na.locf(gold, fromLast = TRUE) 

gold <- ts(gold, start = 1985, frequency = 365)

model <- nnetar(gold)

prediction <- forecast(model, h = 100)

plot(prediction)
