conc <- c(2.856829,5.005303,7.519473,22.101664,27.769976,
          33.198025,45.483269,203.784238)
rate <- c(14.58342,24.74123,31.34551,72.96985,77.50099,
          96.08794,96.96624,108.88374)
data <- data.frame(conc,rate)
data
plot(rate~conc)
f <- formula(rate~vm*conc/(k+conc))
model <- nls(f, data = data, start = list(k = 20,vm = 120),trace = T)
summary(model)
library(nlstools)
contourmodel <- nlsContourRSS(model)
plot(contourmodel, col = F, nlev = 10)
fitted(model)
concVal <- with(data, seq(min(conc),max(conc), length.out = 100))
plot(rate~conc,ylim=c(10,120))
lines(concVal, predict(model,
                         newdata = data.frame(conc = concVal)))
