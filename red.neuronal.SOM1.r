#Agrupación con k-means
datacrude = iris
head(datacrude)
data <- subset(datacrude,select = c(Sepal.Length,Sepal.Width,
                                    Petal.Length,Petal.Width))
head(data)
plot(data)
normalized <- scale(data)
model <- kmeans(normalized,5,algorithm = 'Lloyd',iter.max = 200)
model
plot(normalized, col = model$cluster)
points(model$centers, col = 1:2, pch = 8, cex = 2)
#Agrupación con red neuronal SOM
library(kohonen)
grid <- somgrid(xdim = 1,ydim = 3,topo = 'rectangular')
map <- som(normalized,
           grid = grid,
           alpha = 0.5)
plot(map,type = 'changes')
plot(map,type = 'counts')
results <-data.frame(map$unit.classif)
results
table(results)
