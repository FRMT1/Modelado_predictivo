library(astsa); library(prophet); library(dplyr); library(ggplot2)

data(chicken)

df <- as.data.frame(chicken)
df

fecha_inicio <- as.Date("2001-08-01")
fecha_final <- as.Date("2016-07-01")

complete_seq <- seq.Date(fecha_inicio, fecha_final, by = 'month')
secuencia_formateada <- format(complete_seq, "%Y-%m-%d")
head(secuencia_formateada)

df <- data.frame(
  ds = secuencia_formateada,
  y = as.numeric(chicken)
)
head(df)

model <- prophet(df, yearly.seasonality = T,
                 weekly.seasonality = T,
                 daily.seasonality = T,
                 seasonality.mode = 'multiplicative')

future <- make_future_dataframe(model, periods = 365)

forecast <- predict(model, future)

plot(model, forecast)
