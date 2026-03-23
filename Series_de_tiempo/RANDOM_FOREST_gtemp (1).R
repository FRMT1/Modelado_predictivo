library(randomForest); library(forecast); library(dplyr); library(astsa)

data <- gtemp_ocean
data_df <- as.data.frame(data)
head(data_df)

plot(data, xlab = 'Años',
     ylab = 'Variación de temperatura',
     main = 'Temperatura de los oceanos entre 1850-2023',
     col = 'darkblue')
abline(v = 1990)

#########################################################################333

gtemp_copy <- as.numeric(data)

gtemp_df <- data.frame(
  year = 1850:2023,
  values = gtemp_copy,
  dif1 = lag(gtemp_copy, n = 1),
  dif2 = lag(gtemp_copy, n = 2),
  dif3 = lag(gtemp_copy, n = 3)
)
head(gtemp_df)

gtemp_df <- na.omit(gtemp_df)

set.seed(123)
rf_model <- randomForest(values ~ ., data = gtemp_df,
                         importance = TRUE, na.action = na.omit,
                         mtry = 2, ntree = 200)
rf_model

rf_predict <- predict(rf_model, gtemp_df)

predicted <- ts(rf_predict, frequency = 1, start = 1854)
autoplot(ts(gtemp_df$values, start = 1854), series = 'Observados',
         main = 'Temperatura predicha y real de los oceanos (1853-1990')+
  autolayer(predicted, series = 'RF model')

#############################################################################


