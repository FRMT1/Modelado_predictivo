#Una neurona de entrada

inputs <- c(1,2,3,2.5)

#Tres neuronas en una capa oculta

weights1 <- c(0.2,0.8,-0.5,1)
weights2 <- c(0.5,-0.91,0.26,-0.5)
weights3 <- c(-0.26,-0.27,0.17,0.87)

bias1 <- 2
bias2 <- 3
bias3 <- 0.5

output <- c(weights1[1]*inputs[1]+weights1[2]*inputs[2]+weights1[3]*inputs[3]+weights1[4]*inputs[4]+bias1,
	weights2[1]*inputs[1]+weights2[2]*inputs[2]+weights2[3]*inputs[3]+weights2[4]*inputs[4]+bias2,
	weights3[1]*inputs[1]+weights3[2]*inputs[2]+weights3[3]*inputs[3]+weights3[4]*inputs[4]+bias3)

print(output)

#El equivalente a dotproduct en python

a <- c(1,2,3)
b <- c(2,3,4)
print(sum(a*b))

#Ejemplo con una sola neurona oculta
inputs <- c(1,2,3,2.5)
weights <- c(0.2,0.8,-0.5,1)
bias <- 2

output <- sum(weights*inputs)+bias
print(output)

#El ejemplo anterior de tres neuronas
inputs <- c(1,2,3,2.5)

weights1 <- c(0.2,0.8,-0.5,1)
weights2 <- c(0.5,-0.91,0.26,-0.5)
weights3 <- c(-0.26,-0.27,0.17,0.87)

bias1 <- 2
bias2 <- 3
bias3 <- 0.5

output <- c(sum(weights1*inputs)+bias1,sum(weights2*inputs)+bias2,sum(weights3*inputs)+bias3)

print(output)

#Otra forma de plantear el ejemplo de tres neuronas

weights <- c(weights1 = c(0.2,0.8,-0.5,1),weights2 = c(0.5,-0.91,0.26,-0.5),weights3 = c(-0.26,-0.27,0.17,0.87))
bias <- c(bias1 = 2,bias2 = 3,bias3 = 0.5)
output <- c(sum(weights1*inputs)+bias1,sum(weights2*inputs)+bias2,sum(weights3*inputs)+bias3)

print(output)

#Definicion y multiplicacion de matrices

a <- matrix(c(1,2,3),nrow = 1)
print(a)
b <- matrix(c(2,3,4),nrow = 3)
print(b)
c <- a%*%b
print(c)
