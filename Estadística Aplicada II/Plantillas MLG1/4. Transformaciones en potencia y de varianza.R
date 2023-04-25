#Importo la base de datos
library(readr)
X<- read_csv("preciosCasas.csv")
library(car)
#Gráficas de relación entre la variable dependiente y sus covariables predictoras
par(mfrow=c(1,3))
attach(X)
plot(Size,Price,col="red1",panel.first = grid(),pch=19)
plot(Baths,Price,col="aquamarine4",pch=19,panel.first = grid())
plot(Beds,Price,col="red1",pch=19,panel.first=grid())
#Realizó el modelo
model<- lm(Price~Size+Baths+Beds,data=X)
#Resumen del modelo
summary(model)
#Gráfica para evaluar homocedasticidad y linealidad
library(MASS)
studenti<- studres(model)
ajustados<- fitted.values(model)
par(mfrow=c(1,1))
plot(ajustados,studenti, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="Residuos Estudentizados VS Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti~ajustados), col = "red1")
res<- residuals(model)
library(lmtest)
bptest(model,~Size+I(Size^2)+Baths+Beds+I(Baths^2)+I(Beds^2),data=X)
library(car)
crPlots(model,main="")
#Gráfica y prueba para evaluar normalidad
par(mfrow=c(1,1))
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main="QQPLOT")
shapiro.test(residuals(model))
library(ggfortify)
autoplot(model)
#####################BOX-COX######################################
#Calculo del lambda más óptimo
box.cox<- boxcox(model,lambda=seq(-20,20,length.out = 1000),
       ylab='log-verosimilitud')
#Selección del Lambda
bc<-round(box.cox$x[box.cox$y ==max(box.cox$y)],2)
#Ajuste del modelo
model.box<- lm(I(Price^bc)~Size+Baths+Beds,data=X)
autoplot(model.box)
#Resumen del modelo
summary(model.box)
summary(X)
x.nuevo<- data.frame(Size=1516 ,Beds=3,Baths=1)
predict(model,x.nuevo,interval = 'confidence')
predict(model.box,x.nuevo,interval = 'confidence')^(1/bc)
#Validación de supuestos del modelo con BOX-COX
library(MASS)
studenti.box<- studres(model.box)
ajustados.box<- model.box$fitted.values
par(mfrow=c(1,1))
plot(ajustados.box,studenti.box, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="Residuos Estudentizados VS Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.box~ajustados.box), col = "red1")
bptest(model.box,~Size+I(Size^2)+Baths+Beds+I(Baths^2)+I(Beds^2),data=X)
library(car)
crPlots(model.box,main="")
par(mfrow=c(1,1))
qqPlot(residuals(model.box),xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main="QQPLOT")
shapiro.test(studenti.box)
library(xtable)
library(lmtest)
bptest(model.box,~Size+I(Size^2)+Baths+Beds+I(Baths^2)+I(Beds^2),data=X)
##############################MINIMOS CUADRADOS PONDERADOS#####################
#Estimación varianza
res.mcp<- residuals(model)
varianza<- lm(abs(res.mcp)~Size+Baths+Beds,data=X)
w = 1/varianza$fitted.values^2
model.ponderados<- lm(Price~Size+Baths+Beds,data=X,weights = w)
summary(model.ponderados)
#Validación Supuestos
qqPlot(res.ponderados)
res.ponderados<- residuals(model.ponderados)*sqrt(w)
plot(fitted.values(model.ponderados),res.ponderados,
     xlab='valores ajustados',ylab='residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4")
lines(lowess(res.ponderados~fitted.values(model.ponderados)),col=2,lty=2,lwd=4)
abline(h=0,lty=2,lwd=2)
library(car)
crPlots(model.ponderados)
###########
(predict(model.box,data.frame(Size=1516,Baths=2,Beds=3),interval='confidence'))^1/bc
predict(model,data.frame(Size=1516,Baths=2,Beds=3),interval='confidence')


