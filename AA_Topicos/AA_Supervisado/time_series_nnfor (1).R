library(nnfor)
str(AirPassengers)
head(AirPassengers)
fit1 <- mlp(AirPassengers)
print(fit1)
plot(fit1)

fit2 <- mlp(AirPassengers, allow.det.season = FALSE, hd = c(8,5))
plot(fit2)

frc1 <- forecast(fit1, h = 12)
frc1
frc2 <- forecast(fit2, h = 12)
frc2

plot(frc1)
plot(frc2)

attributes(frc1)
frc1$mean
frc1$fitted
frc1$all.mean
frc1$residuals

fit3 <- elm(AirPassengers)
fit3
plot(fit3)

frc1 <- forecast(fit1, h = 144)
plot(frc1)
