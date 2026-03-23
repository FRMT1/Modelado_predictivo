#Base de datos para predecir la enfermedad cardiovascular 
#en función de 13 predictores
#Fuente: UCI machine learning repository
data <- read.csv('/home/profesor/Modelado predictivo con R/heart.UCI.MLR.dat', sep = "", header = F)
head(data)
#nombrando las columna
colnames(data) <- c('age','sex','cp','bp','scol','fbs','reec',
                    'mratcar','eiang','oldpeak','spexer',
                    'nmv','thal','hd')
#Encabezado y estructura de la base de datos
head(data)
str(data)
#Conversión de la variable dependiente en binaria
data$hd <- ifelse(data$hd == 2,1,0)
str(data)
#Modelo de regresión logística
model <- glm(hd~.,data = data, family = 'binomial')
summary(model)
#Optimización del modelo. Sustracción de variables no significativas
model <- glm(hd~.-age-fbs-reec-oldpeak-spexer, data = data, 
             family = 'binomial')
summary(model)
#Evaluación de la capacidad predictiva y matriz de confusión.
res <- predict(model,data,type = 'response')
table(actual.values = data$hd,predicted.values = res > 0.5)
accuracy <- (132+96)/(132+96+24+18)
accuracy
#Visualización del modelo
predicted.data <- data.frame(
    probability.of.hd = model$fitted.values,
    heart.disease = data$hd)
head(predicted.data)
predicted.data <- predicted.data[order(predicted.data$probability.of.hd,
                                       decreasing = F),]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
library(cowplot)
ggplot(predicted.data,aes(x = rank, 
                          y = probability.of.hd))+
    geom_point(aes(color = heart.disease))+
    xlab("index")+
    ylab("probability of heart disease")


