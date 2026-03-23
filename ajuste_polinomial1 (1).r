#Modelado de muerte microbiana
set.seed(31)
t <- seq(from = 0, to = 30, by = 1)
n <- 500000*exp(-0.12*t)
plot(n~t,type = "l")
noise <- rnorm(length(t), mean = 15, sd = 30000)
noise.n <- n + noise
print(noise.n)
plot(t,noise.n,col = "darkblue",xlab = "Tiempo",
     ylab = "ufc/ml")
model <- lm(noise.n~poly(t, degree = 2))
predictions <- predict(model,data.frame(t),
                       interval = "confidence",
                       level = 0.95)
lines(predictions[,1],col = 'red')
lines(predictions[,2],col = 'black')
lines(predictions[,3],col = 'black')
legend('topright',c("Observados","Predichos"),
       col = c("darkblue","red"),cex = 0.6,lwd = 0.8)
