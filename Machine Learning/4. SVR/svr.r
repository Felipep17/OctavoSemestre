# SVR

# Importar el dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Purchased, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# testing_set = subset(dataset, split == FALSE)


# Escalado de valores
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])

# Ajustar SVR con el Conjunto de Datos
#install.packages("e1071")
library(e1071)
regression = svm(formula = Salary ~ ., 
                 data = dataset, 
                 type = "eps-regression", 
                 kernel = "radial")
summary(regression)
# Predicción de nuevos resultados con SVR 
y_pred = predict(regression, newdata = data.frame(Level = 6.5))

# Visualización del modelo de SVR
# install.packages("ggplot2")
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regression, 
                                        newdata = data.frame(Level = dataset$Level))),
            color = "blue") +
  ggtitle("Predicción (SVR)") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")
########################
#Univariante
library(alr4)
library(ggfortify)
X<- cakes
model<- lm(Y~X1,data =X)
autoplot(model)
summary(model)
plot(Y~X1,data=X)
#############
regression = svm(formula = Y~X1, 
                 data = X, 
                 type = "eps-regression", 
                 kernel = "radial")

# Visualización del modelo de SVR
# install.packages("ggplot2")
library(ggplot2)
x_grid = seq(min(X$X1), max(X$X1), 0.1)
ggplot() +
  geom_point(aes(x = X$X1 , y = X$Y),
             color = "red") +
  geom_line(aes(x = X$X1, y = predict(regression, 
                                               newdata = data.frame(X1 = X$X1))),
            color = "blue") +
  ggtitle("Predicción (SVR)") +
  xlab("X1") +
  ylab("Y")
