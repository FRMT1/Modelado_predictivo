require(quantmod) 
require(nnet)
require(caret)
T = seq(0,20,length=200)
y = 1 + 3*cos(4*T+2) +.2*T^2 + rnorm(200)
dat <- data.frame( y, x1=Lag(y,1), x2=Lag(y,2))
dat
names(dat) <- c('y','x1','x2')
dat <- dat[c(3:200),] #delete first 2 observations
dat
#Fit model
model <- train(y ~ x1+x2 , 
               dat, 
               method='nnet', 
               linout=TRUE, 
               trace = FALSE)
ps <- predict(model, dat)

#Examine results

plot(T,y,type="l",col = 2)
lines(T[-c(1:2)],ps, col=3)


# OTRA FORMA

require(quantmod) 
require(nnet)
require(caret)
t = seq(0,20,length=200)
y = 1 + 3*cos(4*t+2) +.2*t^2 + rnorm(200)
dat <- data.frame( y, x1=Lag(y,1), x2=Lag(y,2))
names(dat) <- c('y','x1','x2')
train_set <- dat[c(3:185),]
test_set <- dat[c(186:200),]
#Fit model
model <- train(y ~ x1+x2 , 
               train_set, 
               method='nnet', 
               linout=TRUE, 
               trace = FALSE)
ps <- predict(model, test_set)

#Examine results

plot(T,y,type="l",col = 2)
lines(T[c(186:200)],ps, col=3)
legend(5, 70, c("y", "pred"), cex=1.5, fill=2:3)
