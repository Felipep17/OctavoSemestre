#Clase 08/02/2022
X<- na.omit(datospesos)#En una matrix X
rownames(X)<- X[,1]
X<- X[,-1]
View(X) #Visualizar
summary(X)
#Función por columna
library(openxlsx)
medias<- as.data.frame(apply(X,2,mean))
colnames(Y)<- c("Media")
write.xlsx(Y,"Promedios.xlsx")
Z<- apply(X,2,min)
apply(X,2,boxplot)
#Promedio a pie
y<-c()
for( i in 1:ncol(X)){
  y[i]<-mean(X[,i])
}
y
## Número datos
nrow(X)
###############
hist(X$length,col="purple",xlab="Longitud del bebé en cm",ylab="Frecuencia Relativa",main="Histograma de la longitud del bebé al nacer",freq=F)
plot(density(X$length),xlab="Longitud del bebé en cm",ylab="Frecuencia Relativa",main="")
lines(density(X$length),lwd=2)
polygon(density(X$length),col="#FFDEAD")
boxplot(X$length)
# Edad
library(zoom) #librería para el zoom
par(mar=c(5,5,5,5)) #márgenes
hist(X$age,col=c("aquamarine1","aquamarine2","aquamarine4"),xlab="Semana gestacional",ylab="Frecuencia Relativa",freq=F,main="",panel.first=grid())
abline(h=0.1) # linea horizontal
abline(v=37) #linea vertical
plot(density(X$age),xlab="Semana gestacional",ylab="Frecuencia Relativa",main="")
lines(density(X$age),lwd=2)
polygon(density(X$age),col="#FFDEAD") #colorear densidad
boxplot(X$age,horizontal = T)
#
ciga<-table(X$mnocig)
barplot(ciga,las=2)
zm()# para hacer zoom
############ 
pairs(X)
attach(X)
plot(length~age,pch=19,panel.first=grid(),xlab="Edad gestacional semanas",ylab="Longitud del bebé en cm")
cor(length,age)
cor(X$length,X$age,method ="spearman")
#MODELO de regresión lineal
model<- lm(length~age,data=X)
summary(model)
coefficients(model)
confint(model)
model$coefficients
model$residuals
model$rank
model$fitted.values
hist(model$residuals)
shapiro.test(model$residuals[model$residuals>-5])
plot(model$residuals)
min(model$residuals)
library(MASS)
studi<- studres(model)
shapiro.test(studi[studi>-3])
###############
abline(model,lwd=2,lty=2)
###################### Continuación
summary(model)
hist(model$residuals)
plot(density(model$residuals))
shapiro.test(model$residuals)
library(car)
qqPlot(model$residuals,pch=19, col=carPalette()[5], col.lines=carPalette()[5],lwd=2)
which(rownames(X)==223)
X[rownames(X)==223,]
plot(length~age,pch=19,panel.first=grid(),xlab="Edad gestacional semanas",ylab="Longitud del bebé en cm")
points(age[41],length[41],col="red",pch=19)
text(age[41],length[41],col="red",labels=rownames(X)[41],pos=1)
#################
plot(model$fitted.values,model$residuals)
abline(h=0,lwd=2,lty=2)
library(MASS)
studi<- studres(model)
plot(model$fitted.values,studi,pch=19,col="aquamarine4",ylim=c(-6,4))
abline(h=0,lwd=2,lty=2)
lines(lowess(studi~model$fitted.values), col = "red1")
library(lmtest)
bptest(model,~age+I(age^2),data=X)
library(ggfortify)
autoplot(model)
crPlots(model)
######################
#Prueba temporalidad y correlación de los errores en el tiempo
par(mfrow=c(1,2))
plot(studi,type="b",xlab="Tiempo",ylab="Residuos Estudentizados",main="A",pch=19)
length(studi)
plot(studi[-nrow(X)],studi[-1],pch=19,panel.first = grid(),col="turquoise3",xlab="Residuos(t-1)",ylab="Residuos(t)",main="B")
abline(lm(studi[-1]~studi[-nrow(X)]))
cor(studi[-1],studi[-nrow(X)])
durbinWatsonTest(model,method='resample',reps=1000)
#####33
Y<- Index2018
plot(Y$spx,xlab="Tiempo",ylab="Residuos Estudentizados",main="A")
#######################################
pairs(X)
pairs(X[,1:4])
summary(model)
modelo<- lm(length~mnocig+age+motherage,data=X)
summary(modelo)
autoplot(modelo)
bptest(model,~age+I(age^2)+weight+I(weight^2)+motherage+I(motherage^2),data=X)
par(mfrow=c(1,1))
qqPlot(modelo)
shapiro.test(residuals(modelo))
###############################
anova(model,modelo)
vif(modelo)
###############################
plot(age~motherage,pch=19)
##############################
# Predicciones modelo reducido
summary(X)
newPoints.1 = cbind(x0=rep(1,4),mnocig=c(10,20,80,5),
                    age=c(35,40,35,35),motherage=c(20,60,15,38))
X.1 = model.matrix(modelo)
XtX.inv.1 = solve(t(X.1)%*%X.1)
h.values = hatvalues(modelo)
hmax.1 = max(h.values)
h0.1 = apply(newPoints.1,1,function(x){t(x)%*%XtX.inv.1%*%x})
h0.1 >hmax.1
################################
predict(modelo,data.frame(mnocig=c(10,5),
                        age=c(35,35),motherage=c(20,38),interval='confidence'))

