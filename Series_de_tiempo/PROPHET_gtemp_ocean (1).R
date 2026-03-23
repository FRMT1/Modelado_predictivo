library(astsa); library(prophet); library(ggplot2)

data("gtemp_ocean")
dataf <- as.data.frame(gtemp_ocean)
head(dataf)

fecha_inicio <- as.Date("1850-01-01")
fecha_final <- as.Date("2023-01-01")

secuencia <- seq.Date(fecha_inicio, fecha_final, by = 'year')
secuencia_formateada <- format(secuencia, "%Y-%m-%d")
head(secuencia_formateada)

df <- data.frame(
  ds = secuencia_formateada,
  y = as.numeric(gtemp_ocean)  # Importante escribir Y
)
head(df)
dim(df)

model <- prophet(df, yearly.seasonality = T,
                 weekly.seasonality = T,
                 daily.seasonality = T, seasonality.mode = 'multiplicative')

future <- make_future_dataframe(model, periods = 20, freq = 'year')
tail(future)

forecast <- predict(model, future)
plot(model, forecast, type = "o", col = "darkblue",
     xlab = 'Años',
     ylab = 'Temperatura (Celsius)')
##################################################################3


