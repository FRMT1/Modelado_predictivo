library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)


ames <- AmesHousing::make_ames()

set.seed(123)
model1 <- train(
    form = Sale_Price~Gr_Liv_Area,
    data = ames,
    method = "lm",
    train_control = trainControl(method = "cv",number = 10)
)
set.seed(123)
model2 <- train(
    form = Sale_Price~Gr_Liv_Area+Year_Built,
    data = ames,
    method = "lm",
    train_control=trainControl(method = "cv",number = 10)
)
set.seed(123)
model3 <- train(
    form = Sale_Price~Gr_Liv_Area+Year_Built+
        Gr_Liv_Area:Year_Built,
    data = ames,
    method = "lm",
    train_control = trainControl(method = "cv",number = 10)
)

###############################################################################

grid <- expand.grid(
  Gr_Liv_Area = seq(min(ames$Gr_Liv_Area), max(ames$Gr_Liv_Area), length.out = 10),
  Year_Built = seq(min(ames$Year_Built), max(ames$Year_Built), length.ou = 10)
)

grid$Sale_Price <- predict(model2, newdata = grid)

p1 <- ggplot(grid, aes(x = Gr_Liv_Area, y = Year_Built, z = Sale_Price))+
  geom_contour_filled()+
  labs(
    title = "Contornos del precio de venta",
    subtitle = "Modelo 2",
    x = "Área habitable",
    y = "Año de construcción",
    fill = "Precio de venta"
  )+
  theme_classic()
  
# Crear una cuadrícula de valores de Gr_Liv_Area y Year_Built
grid <- expand.grid(
  Gr_Liv_Area = seq(min(ames$Gr_Liv_Area), max(ames$Gr_Liv_Area), length.out = 10),
  Year_Built = seq(min(ames$Year_Built), max(ames$Year_Built), length.out = 10)
)

# Agregar la predicción del modelo
grid$Sale_Price <- predict(model3, newdata = grid)

p2 <- ggplot(grid, aes(x = Gr_Liv_Area, y = Year_Built, z = Sale_Price)) +
  geom_contour_filled() +
  labs(
    title = "Contornos del Precio de Venta",
    subtitle = "Modelo 3",
    x = "Área Habitable",
    y = "Año de Construcción",
    fill = "Precio de Venta"
  ) +
  theme_classic()

gridExtra::grid.arrange(p1,p2, nrow = 2)

#####################################################################################

set.seed(123)
model4 <- train(
    form = Sale_Price~ .,
    data = ames,
    method = "lm",
    train_control = trainControl(method = "cv",number = 10)
)

summary(resamples(list(
    model1,
    model2,
    model3,
    model4
)))

summary(model4)

#####################################################################################

df1 <- broom::augment(model4$finalModel, data = ames)
df2 <- mutate(df1, id = row_number())
ggplot(df1, aes(x = .fitted, y = .std.resid))+
  geom_point(size = 1, alpha = 0.4)+
  labs(
    title = "Homogeneidad de la varianza",
    subtitle = "Modelo 4",
    x = "Valores predichos",
    y = "Residuales"
  )

  ggplot(df2, aes(x = id, y = .std.resid))+
    geom_point(size = 1, alpha = 0.4)+
    labs(
      title = "Independencia de los residuales",
      subtitle = "Modelo 4",
      x = "Orden de corrida",
      y = "Residuales"
    )

residuales <- residuals(model4)  


##################################################################################

# El modelo puede mejorarse
# removiendo términos no significativos y transformando la variable respuesta

#####################################################################################

ames$Sale_Price <- log10(ames$Sale_Price)

set.seed(123)
model5 <- train(
  Sale_Price ~ .,
  data = ames,
  method = "lm",
  train_control = trainControl(method = "cv", number = 10)
)

residuales <- residuals(model5)

df3 <- broom::augment(model5$finalModel, data = ames)
df4 <- mutate(df3, id = row_number())

# Homogeneidad de la varianza
ggplot(df3, aes(x = .fitted, y = residuales))+
  geom_point(size = 1, alpha = 0.4)

#Independencia
ggplot(df4, aes(x = id, y = residuales))+
  geom_point(size = 1, alpha = .4)


#######################################################################################3

# El modelo debe ser usado con precaución por cuanto se está asumiendo una relación
# lineal entre la variable dependiente y las independientes.

# Para evitar riesgos, se sugiere implementar otros algoritmos de aprendizaje automático como
# los árboles de decisión.
