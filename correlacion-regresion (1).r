library(agricolae)
set.seed(31)
x <- seq(from = 0, to = 30, by = 0.5)
y <- 0 + 0.5*x
plot(x,y,type = 'l')
noise <- rnorm(length(x),15,0.78)
noise.y <- y + noise
plot(noise.y~x, xlab = "x", ylab = "y")
model <- lm(noise.y~x)
summary(model)
predictions <- predict(model,data.frame(x),
                       interval = "confidence",
                       level = 0.99)
lines(x,predictions[,1],col = "red")
lines(x,predictions[,2],col = "black")
lines(x, predictions[,3],col = "black")
cor(x,noise.y)
analysis <- correlation(x,noise.y,method = 'pearson')


