library(readr)
library(dplyr)
library(caret)

# Pre-procesamiento para trabajar con Keras en COLAB

#Censo US
train <- readr::read_csv("/home/felix/Descargas/adult_processed_train.csv")
View(train)
train <- train %>%
  mutate(dataset = "train")
View(train)
str(train)
head(train)
test <- readr::read_csv("/home/felix/Descargas/adult_processed_test.csv")
test <- test %>%
  mutate(dataset = "test")

#Uniendo ambos datasets

all <- rbind(train,test)
View(all)

#Remoción de NA

all <- all[complete.cases(all),]
#Remoción de espacios en blanco tipo ("Male " = "Male") 

all <- all %>%
  mutate_if(~is.factor(.),~trimws(.))

#REmovidos los NA y los espacios en blanco. Volvemos a separar
#los datos de entrena,iento

train <- all%>%
  filter(dataset == "train")

#El objetivo es un caracter. Lo transformamos a numérico
#Definimos las variables predictoras de entrenamiento

train_target <- as.numeric(factor(train$income))
train <- train %>%
  select(-income,-dataset) 

#Separamos las columnas "Character" de las columnas "integer"

train_chars <- train %>%
  select_if(is.character)
train_integs <- train %>%
  select_if(is.integer)

#Cambiamos las filas donde el caracter está presente por 1
#y por cero si está ausente

ohe <- caret::dummyVars(" ~ .", data = train_chars)
train_ohe <- data.frame(predict(ohe, newdata = train_chars)) 

#Volvemos a colocar los dos datasets juntos para conformar los
#predictores de entrenamiento

train <- cbind(train_ohe, train_integs)

#Se hace exactamente lo mismo para los datos de validación

test <- all %>% filter(dataset == "test")
test_target <- as.numeric(factor(test$income))
test <- test %>% select(-income, -dataset)
test_chars <- test %>%
  select_if(is.character)
test_ints <- test %>%
  select_if(is.integer)
ohe <- caret::dummyVars(" ~ .", data = test_chars)
test_ohe <- data.frame(predict(ohe, newdata = test_chars))
test <- cbind(test_ints,test_ohe)

#Cuando se crea el vector objetivo, el código lo identifica como 1 y 2
#pero queremos que lo identifique como 0 y 1. Por lo tanto restamos 1
#al vector objetivo

train_target <- train_target - 1
test_target <- test_target - 1


