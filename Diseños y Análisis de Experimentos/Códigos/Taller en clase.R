y <- c(36, 41, 39, 42, 49)
x <- c(20, 25, 24, 25, 32)
x2 <- c(22, 28, 22, 30, 28)
y2 <- c(40, 48, 39, 45, 44)
x3 <- c(21, 23, 26, 21, 15)
y3 <- c(35, 37, 42, 34, 32)
sxx <- sum((x - mean(c(x, x2, x3)))^2, (x2 - mean(c(x, x2, x3)))^2, (x3 - mean(c(x, x2, x3)))^2)
syy <- sum((y - mean(c(y, y2, y3)))^2, (y2 - mean(c(y, y2, y3)))^2, (y3 - mean(c(y, y2, y3)))^2)
sxy <- sum(((x - mean(c(x, x2, x3)))) * (y - mean(c(y, y2, y3))) +
  ((x2 - mean(c(x, x2, x3)))) * (y2 - mean(c(y, y2, y3))) +
  ((x3 - mean(c(x, x2, x3)))) * (y3 - mean(c(y, y2, y3))))
ssep <- syy - sxy^2 / sxx
exx <- sum((x - mean(c(x)))^2, (x2 - mean(c(x2)))^2, (x3 - mean(c(x3)))^2)
eyy <- sum((y - mean(c(y)))^2, (y2 - mean(c(y2)))^2, (y3 - mean(c(y3)))^2)
exy <- sum(((x - mean(c(x)))) * (y - mean(c(y))) +
  ((x2 - mean(c(x2)))) * (y2 - mean(c(y2))) +
  ((x3 - mean(c(x3)))) * (y3 - mean(c(y3))))
sse <- eyy - (exy^2) / exx
Tabla <- matrix(c(sxy^2 / sxx, 1, 0, 0, ssep - sse, 2, (ssep - sse) / 2, ((ssep - sse) / (2)) / (sse / 11), sse, 11, sse / 11, 0, syy, 14, 0, 0), nrow = 4, ncol = 4, byrow = T)
colnames(Tabla) <- c("Sum of Squares", " Degrees of Freedom", " Mean Square", "F0")
rownames(Tabla) <- c("Regression", "Treatments", "Error", "Total")
View(Tabla)
### Tabla 2
txx<- 5*(sum((mean(x) - mean(c(x, x2, x3)))^2, (mean(x2) - mean(c(x, x2, x3)))^2, (mean(x3) - mean(c(x, x2, x3)))^2))

tyy<- 5*(sum((mean(y) - mean(c(y, y2, y3)))^2, (mean(y2) - mean(c(y, y2, y3)))^2, (mean(y3) - mean(c(y, y2, y3)))^2))
txy <- 5*(sum(((mean(x) - mean(c(x, x2, x3)))) * (mean(y) - mean(c(y, y2, y3))) +
             ((mean(x2) - mean(c(x, x2, x3)))) * (mean(y2) - mean(c(y, y2, y3))) +
             ((mean(x3) - mean(c(x, x2, x3)))) * (mean(y3) - mean(c(y, y2, y3)))))

Tabla2 <- matrix(c(2, txx, txy, tyy, 0, 0, 0,12, exx, exy, eyy,sse, 11, sse/11, 14, sxx,sxy,syy,ssep,13,0,0,0,0,0,ssep-sse,2,(ssep-sse)/2), nrow = 4, ncol = 7, byrow = T)
rownames(Tabla2)<- c("Treatments","Error","Total","Adjusted Treatment")
colnames(Tabla2)<- c("Degrees of Freedom","x","xy","y","y","Degrees of Freedom","Mean Square")
View(Tabla2)
# Párametros
mu<- mean(c(y,y2,y3))
taus<- c(mean(y)-mu,mean(y2)-mu,mean(y3)-mu)
beta<- exy/exx


pvalue y <- c(36, 41, 39, 42, 49)
x <- c(20, 25, 24, 25, 32)
x2 <- c(22, 28, 22, 30, 28)
y2 <- c(40, 48, 39, 45, 44)
x3 <- c(21, 23, 26, 21, 15)
y3 <- c(35, 37, 42, 34, 32)
sxx <- sum((x - mean(c(x, x2, x3)))^2, (x2 - mean(c(x, x2, x3)))^2, (x3 - mean(c(x, x2, x3)))^2)
syy <- sum((y - mean(c(y, y2, y3)))^2, (y2 - mean(c(y, y2, y3)))^2, (y3 - mean(c(y, y2, y3)))^2)
sxy <- sum(((x - mean(c(x, x2, x3)))) * (y - mean(c(y, y2, y3))) +
             ((x2 - mean(c(x, x2, x3)))) * (y2 - mean(c(y, y2, y3))) +
             ((x3 - mean(c(x, x2, x3)))) * (y3 - mean(c(y, y2, y3))))
ssep <- syy - sxy^2 / sxx
exx <- sum((x - mean(c(x)))^2, (x2 - mean(c(x2)))^2, (x3 - mean(c(x3)))^2)
eyy <- sum((y - mean(c(y)))^2, (y2 - mean(c(y2)))^2, (y3 - mean(c(y3)))^2)
exy <- sum(((x - mean(c(x)))) * (y - mean(c(y))) +
             ((x2 - mean(c(x2)))) * (y2 - mean(c(y2))) +
             ((x3 - mean(c(x3)))) * (y3 - mean(c(y3))))
sse <- eyy - (exy^2) / exx
Tabla <- matrix(c(sxy^2 / sxx, 1, 0, 0, ssep - sse, 2, (ssep - sse) / 2, ((ssep - sse) / (2)) / (sse / 11), sse, 11, sse / 11, 0, syy, 14, 0, 0), nrow = 4, ncol = 4, byrow = T)
colnames(Tabla) <- c("Sum of Squares", " Degrees of Freedom", " Mean Square", "F0")
rownames(Tabla) <- c("Regression", "Treatments", "Error", "Total")
View(Tabla)
### Tabla 2
txx<- 5*(sum((mean(x) - mean(c(x, x2, x3)))^2, (mean(x2) - mean(c(x, x2, x3)))^2, (mean(x3) - mean(c(x, x2, x3)))^2))

tyy<- 5*(sum((mean(y) - mean(c(y, y2, y3)))^2, (mean(y2) - mean(c(y, y2, y3)))^2, (mean(y3) - mean(c(y, y2, y3)))^2))
txy <- 5*(sum(((mean(x) - mean(c(x, x2, x3)))) * (mean(y) - mean(c(y, y2, y3))) +
                ((mean(x2) - mean(c(x, x2, x3)))) * (mean(y2) - mean(c(y, y2, y3))) +
                ((mean(x3) - mean(c(x, x2, x3)))) * (mean(y3) - mean(c(y, y2, y3)))))

Tabla2 <- matrix(c(2, txx, txy, tyy, 0, 0, 0,12, exx, exy, eyy,sse, 11, sse/11, 14, sxx,sxy,syy,ssep,13,0,0,0,0,0,ssep-sse,2,(ssep-sse)/2), nrow = 4, ncol = 7, byrow = T)
rownames(Tabla2)<- c("Treatments","Error","Total","Adjusted Treatment")
colnames(Tabla2)<- c("Degrees of Freedom","x","xy","y","y","Degrees of Freedom","Mean Square")
View(Tabla2)
# Párametros
mu<- mean(c(y,y2,y3))
taus<- c(mean(y)-mu,mean(y2)-mu,mean(y3)-mu)
beta<- exy/exx

pvalue <- pf(2.610643,2,11,lower.tail = F)
 