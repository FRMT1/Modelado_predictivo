library(readr)
data <- readr::read_csv("/home/felix/Modelado predictivo/AA_Topicos/AA_No_Supervisado/protein.csv")
View(data)
str(data)

library(corrr)
library(FactoMineR)
library(factoextra)
library(ggcorrplot)

colSums(is.na(data))

numerical_data <- data[,2:10]

head(numerical_data)

data_normalized <- scale(numerical_data)
head(data_normalized)

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]

fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

fviz_cos2(data.pca, choice = "var", axes = 1:2)

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
