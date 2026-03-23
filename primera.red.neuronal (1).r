library(neuralnet)
datos = read.csv("/home/felix/Modelado predictivo con R/squares.csv",
                 sep = ",",header = TRUE)
attach(datos)
names(datos)
datos
model=neuralnet(formula = output~input,
                data = datos,
                hidden = 10,
                threshold = 0.01)
print(model)
plot(model)
final_output=cbind(input,output,
                   as.data.frame(model$net.result))
colnames(final_output)=c("input","expected output",
                         "neural net output")
print(final_output)
plot(final_output$`neural net output`~final_output$`expected output`,
         xlab="Observados",ylab = "Predichos")
abline(lm(final_output$`neural net output`~
              final_output$`expected output`))

