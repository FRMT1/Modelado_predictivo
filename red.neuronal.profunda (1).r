library(ISLR)
library(neuralnet)
crudedata = College
head(crudedata)
str(crudedata)
private <- as.numeric(crudedata$Private)
data <- cbind(private,crudedata[2:18])
head(data)
normalize <- function(x){
    (x-min(x))/(max(x)-min(x))
}
data_normalized <- sapply(data,FUN = normalize)
head(data_normalized)
sample <- sample(1:nrow(data),
                 round(0.70*nrow(data)))
sample
training_data <- as.data.frame(data_normalized[sample,])
str(training_data)
test_data <- as.data.frame(data_normalized[-sample,])
str(test_data)
f <- as.formula(private~Apps+Accept+Enroll+Top10perc+Top25perc+
                    F.Undergrad+P.Undergrad+Outstate+Room.Board+
                    Books+Personal+PhD+Terminal+S.F.Ratio+
                    perc.alumni+Expend+Grad.Rate)
f
deep_net <- neuralnet(f,data = training_data,
                      hidden = c(6,4))
plot(deep_net,show.weights = FALSE,col.entry = "blue",
     col.hidden = "red", col.out = "blue")
predicted_data <- compute(deep_net,test_data[,2:18])
head(predicted_data$net.result)
predicted_data$net.result <- sapply(
    predicted_data$net.result,round,digits=0
)
results <- data.frame(Actual=test_data[1],
                             Predicho=predicted_data$net.result)
results
cont_table <- table(results)
cont_table
Accuracy <- ((55+164)/(7+164+55+7))*100
Accuracy
aciertos =55+164
aciertos
errores=7+6+1
errores
total=aciertos+errores
total


