library(mgcv)
par(mfrow = c(2,2),mar = c(1,1,1,1))
x <- seq(-.5,1.5,length = 60)
y <- x
f1 <- function(x,y,k = 15){
    r <- sqrt(x**2+y**2);
    f2 <- -exp(-r**2*k);
    f2
}
f3 <- outer(x,y,f1)
persp(x,y,f3,theta = 30, phi = 30, col = "lightblue")
n <- 2000
x <- runif(n)*2-.5
y <- runif(n)*2-.5
f4 <- f1(x,y)
f <- f4+rnorm(n)*.1
model1 <- gam(f~s(x,y,k = 150),gamma = 2)
vis.gam(model1, theta = 30, phi = 30, col = "lightblue")
model2 <- gam(f~s(x,y,bs = "ad",k = 15,m = 1),gamma = 1.4)
vis.gam(model2, theta = 30, phi = 30, col = "lightblue")
model3 <- gam(f~s(x,y,bs = "ad",k = 15, m = 3),gamma = 2)
vis.gam(model3, theta = 30, phi = 30, col = "lightblue")
cor(fitted(model1),f);cor(fitted(model2),f);cor(fitted(model3),f)
