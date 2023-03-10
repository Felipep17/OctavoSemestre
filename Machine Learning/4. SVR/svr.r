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
#Dos variables
regression = svm(formula = Y~X1+X2, 
                 data = X, 
                 type = "eps-regression", 
                 kernel = "radial")
summary(regression)
#####################################
##############################
cakes$X1c = cakes$X1 - mean(cakes$X1)
cakes$X2c = cakes$X2 - mean(cakes$X2)
modc.cakes = lm(Y ~ X1c*X2c + I(X1c^2)+I(X2c^2),data=cakes)
summary(modc.cakes)
X1 = seq(32, 38, length.out = 50)
X2 = seq(335, 365, length= 50)

y <- outer(X= X1, Y = X2, FUN = function(x, y) {
  predict(modc.cakes, newdata = data.frame(X1c = x-mean(cakes$X1), X2c = y-mean(cakes$X2)))
})

contour(X1, X2, y,xlab='tiempo de horneado (minutos)',
        ylab='temperatura de horneado (Fahrenheit)')
bivn.kde<- kde2d(cakes$X1c,cakes$X2c)
?kde2d
