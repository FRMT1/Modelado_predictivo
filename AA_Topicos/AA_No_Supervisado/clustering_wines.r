library(rattle)
library(factoextra)
library(FactoMineR)

data <- wine
View(data)

data <- data[,2:14]
str(data)

sum(is.na(data))

m <- apply(data,2, FUN = mean)
s <- apply(data,2, FUN = sd)
z <- scale(data, center = m, scale = s)
View(z)

cluster_optimo <- fviz_nbclust(z, FUNcluster = hcut, method = "wss")
plot(cluster_optimo)
k <- 3

HCLUSTER <- eclust(z, FUNcluster = "hclust", k = k, graph = TRUE)
fviz_dend(HCLUSTER)

HCLUSTER_clusters <- HCLUSTER$cluster
HCLUSTER_clusters

KMCLUSTER <- eclust(z, FUNcluster = "kmeans", k = k, graph = TRUE)
KMCLUSTER_clusters <- KMCLUSTER$cluster
KMCLUSTER_clusters
KMCLUSTER_means <- KMCLUSTER$centers
KMCLUSTER_means

pca <- PCA(z, graph = TRUE)
plot(pca$ind$coord[,1:2], pch=16, col=HCLUSTER_clusters, 
     main="HIERARCHICAL")
plot(pca$ind$coord[,1:2], pch=16, col=KMCLUSTER_clusters, 
     main="KMEANS")
