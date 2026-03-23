library(ggplot2)
library(gridExtra)
head(iris)
plot1 <- ggplot(iris,aes(Sepal.Length,Sepal.Width,color = Species))+
    geom_point(size = 2)+
    ggtitle("Sepal features")
plot2 <- ggplot(iris,aes(Petal.Length,Petal.Width,color = Species))+
    geom_point(size = 2)+
    ggtitle("Petal features")
grid.arrange(plot1,plot2,ncol = 2)
iris.cluster <- kmeans(iris[,3:4],3)
table(iris.cluster$cluster,species = iris$Species)
plot3 <- ggplot(iris,aes(Petal.Length,Petal.Width,
                         color = iris.cluster$cluster))+
    geom_point(size = 2)+
    ggtitle("Clusters")
grid.arrange(arrangeGrob(plot1,plot2,ncol = 2),plot3)

