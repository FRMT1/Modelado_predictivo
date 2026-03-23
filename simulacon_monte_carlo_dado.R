par(mfrow = c(2,2))
set.seed(123)
lanzamientos1 <- sample(1:6, 5, replace = T)
probabilidades1 <- table(lanzamientos1)/5
barplot(probabilidades1, main = "Simulación Monte Carlo 1 (n=5)",
        xlab = "Caras del dado",
        ylab = "Probabilidad",
        ylim = c(0,.40))
abline(h = 1/6, col = "blue", lty = 2)
set.seed(123)
lanzamientos2 <- sample(1:6, 20, replace = T)
probabilidades2 <- table(lanzamientos2)/20
barplot(probabilidades2, main = "Simulación Monte Carlo 2 (n=20)",
        xlab = "Caras del dado",
        ylab = "Probabilidad",
        ylim = c(0,.40))
abline(h = 1/6, col = "blue", lty = 2)
set.seed(123)
lanzamientos3 <- sample(1:6, 80, replace = T)
probabilidades3 <- table(lanzamientos3)/80
barplot(probabilidades3, main = "Simulación Monte Carlo 3 (n=80)",
        xlab = "Caras del dado",
        ylab = "Probabilidad",
        ylim = c(0,.40))
abline(h = 1/6, col = "blue", lty = 2)
set.seed(123)
lanzamientos4 <- sample(1:6, 1000, replace = T)
probabilidades4 <- table(lanzamientos4)/1000
barplot(probabilidades4, main = "Simulación Monte Carlo 4 (n=1000)",
        xlab = "Caras del dado",
        ylab = "Probabilidad",
        ylim = c(0,.40))
abline(h = 1/6, col = "blue", lty = 2)


