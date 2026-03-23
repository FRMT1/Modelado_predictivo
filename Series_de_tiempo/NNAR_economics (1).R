library(forecast); library(ggplot2)

df <- data.frame(
  date = economics$date,
  y = economics$unemploy
)
head(df)
tail(df)

data <- economics$unemploy

data_ts <- ts(data, start = c(1967,7), end = c(2015,4), frequency = 12)

autoplot(data_ts)+
  ggtitle("Desempleo (1967-2015)")+
  xlab("Años")+
  ylab("Nº desempleados")

model_nnetar <- nnetar(data_ts)  # Revisar luego otros argumentos

model_nnetar

periodos_futuros <- 72

futuro_predicted <- forecast(model_nnetar, h = periodos_futuros)

futuro_predicted

autoplot(futuro_predicted)+
  autolayer(fitted(futuro_predicted), series = "Valores ajustados")+
  ggtitle("Pronóstico de desempleo a 5 años")+
  xlab("Años")+
  ylab("Nº desempleados")+
  guides(colour = guide_legend("Serie de tiempo"))+
  theme(legend.position = "bottom")

##############################################################################

# El ajuste con las redes neuronales autorregresivas es excelente.
