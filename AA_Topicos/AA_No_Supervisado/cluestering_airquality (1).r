air <- airquality
View(air)
sum(is.na(air))
air <- na.omit(air)
str(air)

data <- air[,1:4]
View(data)

m <- apply(data,2,FUN = mean)
s <- apply(data,2, FUN = sd)
z <- scale(data, center = m, scale = s)
View(z)

distances <- dist(z, method = "euclidean")
distances

hcc <- hclust(distances, method = "complete")
plot(hcc, labels = data$Month)
hca <- hclust(distances, method = "average")
plot(hca, labels = data$Month)

members_c <- cutree(hcc,7)
members_a <- cutree(hca,7)
table(members_a,members_c)

aggregate(z, list(members_c), mean)

kmc <- kmeans(z,7)
kmc
kmc$size
plot(air$Solar.R,air$Temp,col = kmc$cluster)
