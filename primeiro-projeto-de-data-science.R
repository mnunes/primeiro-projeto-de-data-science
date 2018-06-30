## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, eval=TRUE, tidy=TRUE, tidy.opts=list(blank=FALSE, width.cutoff=60))
set.seed(1234)

## ----pacotes, eval=FALSE-------------------------------------------------
## install.packages(c("tidyverse", "corrplot", "GGally", "caret", "rpart", "rpart.plot"), dependencies=TRUE)

## ----eda01---------------------------------------------------------------
# definir o endereco do conjunto de dados e baixa-lo

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"

iris        <- read.csv(url, header=FALSE)
names(iris) <- c("c_sepala", "l_sepala", "c_petala", "l_petala", "especie")

# remover a string 'Iris-' do inicio de cada tipo de especie

iris$especie <- as.factor(gsub("Iris-", "", iris$especie))

# estatistica descritiva basica

summary(iris)

## ----eda02---------------------------------------------------------------
library(corrplot)

correlacao <- cor(iris[, -5])

corrplot.mixed(correlacao, upper="ellipse")

## ----eda03---------------------------------------------------------------
library(tidyverse)
theme_set(theme_bw())

ggplot(iris, aes(x=c_petala, y=l_petala)) +
  geom_point()

## ----eda04---------------------------------------------------------------
ggplot(iris, aes(x=c_petala, y=l_petala, colour=especie)) +
  geom_point()

## ----eda05---------------------------------------------------------------
library(GGally)

# remover a variavel especie, pois nao eh quantitativa

ggpairs(iris[, -5], aes(colour = iris$especie))

## ----arvore--------------------------------------------------------------
library(rpart)
library(rpart.plot)
modelo <- rpart(especie ~ ., method="class", data=iris)
prp(modelo, extra=1)

## ----sobreajuste, echo=FALSE, warning=FALSE------------------------------
ajuste_lm   <- lm(dist ~ speed, data=cars)
ajuste_poly <- lm(dist ~ poly(speed, 20, raw=TRUE), data=cars)

cars2 <- data.frame(cars, ajuste_lm=predict(ajuste_lm, cars), ajuste_poly=predict(ajuste_poly, cars))

cars2$grupo <- NA
cars2$grupo <- ifelse(predict(ajuste_poly, cars) > cars$dist, "g1", "g2")

ggplot(cars2, aes(x=speed, y=dist)) +
  geom_point(aes(colour=grupo), show.legend = FALSE) +
  geom_line(aes(y=ajuste_lm, linetype="Ajustado")) +
  geom_line(aes(y=ajuste_poly, linetype="Sobreajustado")) +
  labs(x="X", y="Y", linetype="Modelo")

## ----data_split----------------------------------------------------------
library(caret)

# definir 75% dos dados para treino, 25% para teste

trainIndex  <- createDataPartition(iris$especie, p=0.75, list=FALSE)
iris_treino <- iris[ trainIndex, ]
iris_teste  <- iris[-trainIndex, ]

## ----ajuste01------------------------------------------------------------
fitControl <- trainControl(method = "cv",
                           number = 5)

## ----ajuste02------------------------------------------------------------
ajuste_iris <- train(especie ~ ., 
                     data = iris_treino, 
                     method = "rf", 
                     importance = TRUE,
                     trControl = fitControl)

## ----ajuste03------------------------------------------------------------
ajuste_iris

## ----ajuste04------------------------------------------------------------
predicao <- predict(ajuste_iris, iris_teste)
confusionMatrix(predicao, iris_teste$especie)

## ----ajuste05------------------------------------------------------------
ggplot(varImp(ajuste_iris))

