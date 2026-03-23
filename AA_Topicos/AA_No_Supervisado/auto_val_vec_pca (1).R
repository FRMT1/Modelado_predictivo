 mat_cov <- matrix(c(     # Matriz de covarianzas
  0.370,0.602,0.149,0.044,0.107,0.209,
  0.602,2.629,0.801,0.666,0.103,0.377,
  0.149,0.801,0.458,0.011,-0.013,0.120,
  0.044,0.666,0.011,1.474,0.252,-0.054,
  0.107,0.103,-0.013,0.252,0.488,-0.036,
  0.209,0.377,0.120,-0.054,-0.036,0.324),
  nrow = 6, byrow = TRUE)
mat_cov

tot_var <- sum(diag(mat_cov))  # Varianza total
tot_var

# Càlculo de autovalores (eigenvalores)

eigenvalores <- eigen(mat_cov)$values
eigenvalores

# Proporción de la varianza total

prop_var <- eigenvalores/tot_var
prop_var

# Proporción acumulada

prop_acum <- c(0.57868854,(0.57868854+0.23930927),
                  (0.57868854+0.23930927+0.08285167),
               (0.57868854+0.23930927+0.08285167+0.05654688),
               (0.57868854+0.23930927+0.08285167+0.05654688+0.02728312),
               (0.57868854+0.23930927+0.08285167+0.05654688+0.02728312+
                  0.01532053))
prop_acum

# Visualizando todo en un data frame

df1 <- data.frame(
  eigenvalores,
  prop_var,
  prop_acum
)
df1

# Los primeros tres componentes principales explican el 90%
# de la variabilidad total de los datos.

# Calculando los eigenvectores

eigenvectores <- data.frame(eigen(mat_cov)$vectors)
library(dplyr)
df2 <- eigenvectores %>%
  select(X1,X2)
df2 <- df2*(-1)
df2

# Primer componente principal (formado con el primer eigenvector)
z1 = 0.207*y1 + 0.87*y2 + 0.26*y3 + 0.32*y4 + 0.06*y5 + 0.12*y6

# Igualmente, los coeficientes del segundo componente son
#el segundo eigenvector