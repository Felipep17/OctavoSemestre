#Primer punto
library(readr)
library(car)
X <- read_csv("ChemReact.csv")
par(mfrow=c(1,2))
attach(X)
plot(Yield~Time,pch=19,col='red1',ylab='Rendimiento',xlab='Tiempo')
plot(Yield~Temp,pch=19,col='aquamarine1',ylab='Rendimiento',xlab='Temperatura')

X$Temp.c<- Temp-mean(Temp)
X$Time.c<- Time-mean(Time)
model<- lm(Yield~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2)+Time.c*Temp.c,data=X)
summary(model)
model.in<- lm(Yield~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2),data=X)
anova(model,model.in)
summary(model.in)
summary(X)
predict(model.in,newdata=data.frame(Temp.c=(1)),(Time.c=1))
X1 = seq(32, 38, length.out = 50)
X2 = seq(335, 365, length= 50)
mean(Temp)
mean(Time)
y <- outer(X= X$Time, Y = X$Temp, FUN = function(x, y) {
  predict(model, newdata = data.frame(Time.c = x-mean(Time), Temp.c = y-mean(Temp)))
})
############################# Curva
library(scatterplot3d)
library(plot3D)
library(plotly)
library(scatterplot3d)
library(rgl)
library(plot3Drgl)
z<-X$Yield
y<-X$Temp.c
x<-X$Time.c
scatter3D(x, y, z, phi = 0, bty = "b",
          pch = 20, cex = 2, ticktype = "detailed")
#La variable Z es la variable a predecir
#Creamos un objeto para realizar las predicciones con elmodelo
objr<-lm(z ~ x+y+I(x^2)+I(y^2))
summary(objr)
#preparamos el modelado 3d
grid.lines = 42
x.pred <- seq(min(x), max(x), length.out = grid.lines)


y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(objr, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# Marcamos las líneas de iteracción para que busquen la recta de regresión
fitpoints <- predict(objr)
#ploteamos la gráfica en 3d con recta de regresión
scatter3D(x, y, z, pch = 19, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "",col = c('aquamarine1','aquamarine2','aquamarine3','aquamarine4'))
#Gráfico dinámico
plotrgl()
########### Contorno

################################ Model
######Validación de supuestos
#Gráfica de R
#Evaluación de heterocedasticidad
par(mfrow=c(1,1))
library(MASS)
studenti<- studres(model)
ajustados<- fitted.values(model)
plot(ajustados,studenti, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="Residuos Estudentizados VS Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti~ajustados), col = "red1")
library(lmtest)
bptest(model,~Temp+Time+I(Time^2)+I(Temp^2)+Time*Temp,data=X)
#Gráfica utilizando 
library(ggfortify)
par(mfrow=c(1,1))
#Residuos parciales para evaluar Linealidad y asegurar el supuesto de que la esperanza de los errores es igual a 0
#Gráfico de normalidad
#Prueba de normalidad
res<- residuals(model)
shapiro.test(res)
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19)
summary(model)
#Correción de normalidad
#Calculo del lambda más óptimo
box.cox<- boxcox(model,lambda=seq(-4,4,length.out = 1000),
                 ylab='log-verosimilitud')
#Selección del Lambda
bc<-round(box.cox$x[box.cox$y ==max(box.cox$y)],2)
#Ajuste del modelo
model.box<- lm(I(Yield^bc)~Temp+Time+I(Time^2)+I(Temp^2)+Time*Temp,data=X)
#Resumen del modelo
summary(model.box)
#Validación de supuestos del modelo con BOX-COX
library(MASS)
studenti.box<- studres(model.box)
ajustados.box<- model.box$fitted.values
par(mfrow=c(1,1))
plot(ajustados.box,studenti.box, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="Residuos Estudentizados VS Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.box~ajustados.box), col = "red1")
bptest(model.box,~Temp+Time+I(Time^2)+I(Temp^2)+Time*Temp,data=X)
par(mfrow=c(1,1))
qqPlot(residuals(model.box),xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main="QQPLOT")
shapiro.test(residuals(model.box))
summary(model.box)
model.box2<- lm(I(Yield^bc)~Temp+Time+I(Time^2)+I(Temp^2),data=X)
anova(model.box,model.box2)
summary(model.box2)
################### MCP##################
#Estimación varianza
res.mcp<- residuals(model)
varianza<- lm(abs(res.mcp)~Temp+Time+I(Time^2)+I(Temp^2)+Time*Temp,data=X)
w = 1/varianza$fitted.values^2
model.ponderados<- lm(Yield~Temp+Time+I(Time^2)+I(Temp^2)+Time*Temp,weights = w)
summary(model.ponderados)
#Validación Supuestos
res.ponderados<- residuals(model.ponderados)*sqrt(w)
qqPlot(res.ponderados)

plot(fitted.values(model.ponderados),res.ponderados,
     xlab='valores ajustados',ylab='residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4")
lines(lowess(res.ponderados~fitted.values(model.ponderados)),col=2,lty=2,lwd=4)
abline(h=0,lty=2,lwd=2)
plot(fitted.values(model.ponderados),abs(res.ponderados),
     xlab='valores ajustados',ylab='| residuos ponderados |',pch=19,panel.first = grid())
lines(lowess(abs(res.ponderados)~fitted.values(model.ponderados)),col=2,pch=19,lwd=4,lty=2)
library(car)
crPlots(model.ponderados)
#Segundo Punto
#Importó la librería e inicializó un vector con la información
library(alr4)
X<- oldfaith
#Evaluó el tipo de relación entre las variables:
par(mfrow=c(1,1))
#Estimo el modelo
plot(X$Duration,X$Interval,pch=19,col="turquoise3",panel.first=grid(),xlab="Duración en segundos",ylab="Intervalo en minutos",
     main='')
points(X$Duration[X>180],X$Interval[X$Duration>180],col='red1',pch=19)
#####Gráfica para el intervalo de confianza y predicción##
#Estimación del modelo
model<- lm(Interval~Duration,data=X)

summary(model)
#Creación del data.frame con los valores para el modelo
x.nuevo = data.frame(Duration=seq(min(X[,1]),max(X[,1]),1))
#Predicción del intervalo de confianza
pred.media = predict(model,x.nuevo,interval = 'confidence')
#Predicción del intervalo de predicción
pred.nuev.obs= predict(model,x.nuevo,interval = 'prediction')
#Gráficas de las lineas
lines(x.nuevo$Duration,pred.media[,2],lty=2,lwd=3)
lines(x.nuevo$Duration,pred.media[,3],lty=2,lwd=3)
lines(x.nuevo$Duration,pred.nuev.obs[,2],lty=3,lwd=3)
lines(x.nuevo$Duration,pred.nuev.obs[,3],lty=3,lwd=3)
#Caja de enunciados
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       lty = c(1, 2,3),pt.cex=2,
       box.lwd=0.6,text.font =15,cex=1)
#Gráfica del modelo
abline(model,lwd=2)
##################### Empezamos con la validación de los supuestos.
X$Duration.x = X$Duration - 180
X$Duration.x[X$Duration < 180] = 0
mod.segmento = lm(Interval~.,data=X)
summary(mod.segmento)
anova(model,mod.segmento)
############
#Estimo el modelo
b.lotes<- coefficients(mod.segmento)
#

plot(X$Duration,X$Interval,pch=19,col="turquoise3",panel.first=grid(),xlab="Duración en segundos",ylab="Intervalo en minutos",
     main='')
points(X$Duration[X>180],X$Interval[X$Duration>180],col='red1',pch=19)
x = c(80,180)
lines(x,b.lotes[1]+x*b.lotes[2],lwd=2)
x = c(180,350)

lines(x,b.lotes[1]-180*b.lotes[3]+x*(b.lotes[2]+b.lotes[3]),lwd=2)
abline(v=180,lty=2,lwd=2)
####################
#Creación del data.frame con los valores para el modelo
x.nuevo = data.frame(Duration=seq(80,180,1),Duration.x=rep(0,101))
#Predicción del intervalo de confianza
pred.media = predict(mod.segmento,x.nuevo,interval = 'confidence')
#Predicción del intervalo de predicción
pred.nuev.obs= predict(mod.segmento,x.nuevo,interval = 'prediction')
#Gráficas de las lineas
lines(x.nuevo$Duration,pred.media[,2],lty=2,lwd=3)
lines(x.nuevo$Duration,pred.media[,3],lty=2,lwd=3)
lines(x.nuevo$Duration,pred.nuev.obs[,2],lty=3,lwd=3)
lines(x.nuevo$Duration,pred.nuev.obs[,3],lty=3,lwd=3)

#############
predict(mod.segmento,data.frame(Duration=180,Duration.x=0))
#Creación del data.frame con los valores para el modelo
x.nuevo1 = data.frame(Duration=seq(180,330,length.out=100),Duration.x=seq(180,330,length.out=100)-180)
#Predicción del intervalo de confianza
pred.media = predict(mod.segmento,x.nuevo1,interval = 'confidence')
#Predicción del intervalo de predicción
pred.nuev.obs= predict(mod.segmento,x.nuevo1,interval = 'prediction')
#Gráficas de las lineas
lines(seq(180,330,length.out=100),pred.media[,2],lty=2,lwd=3)
lines(seq(180,330,length.out=100),pred.media[,3],lty=2,lwd=3)
lines(seq(180,330,length.out=100),pred.nuev.obs[,2],lty=3,lwd=3)
lines(seq(180,330,length.out=100),pred.nuev.obs[,3],lty=3,lwd=3)
#Caja de enunciados
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       lty = c(1, 2,3),pt.cex=2,
       box.lwd=0.6,text.font =15,cex=1)
#Gráfica de R
#Evaluación de heterocedasticidad
par(mfrow=c(1,1))
library(MASS)
studenti<- studres(mod.segmento)
ajustados<- fitted.values(mod.segmento)
plot(ajustados,studenti, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="Residuos Estudentizados VS Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti~ajustados), col = "red1")
library(lmtest)
bptest(model,~Duration+I(Duration^2)+I(Duration.x^2),data=X)
#Gráfica utilizando 
library(ggfortify)
par(mfrow=c(1,1))
#Residuos parciales para evaluar Linealidad y asegurar el supuesto de que la esperanza de los errores es igual a 0
par(mfrow=c(1,1))
crPlots(mod.segmento,pch=19,xlab="Duración en segundos")
#Gráfico de normalidad
#Prueba de normalidad
res<- residuals(mod.segmento)
shapiro.test(res)
qqPlot(mod.segmento,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19)
#Prueba temporalidad y correlación de los errores en el tiempo
par(mfrow=c(1,2))
plot(studenti,type="b",xlab="Tiempo",ylab="Residuos Estudentizados",main="A")
length(studenti)
plot(studenti[-270],studenti[-1],pch=19,panel.first = grid(),col="turquoise3",xlab="Residuos(t-1)",ylab="Residuos(t)",main="B")
abline(lm(studenti[-1]~studenti[-270]))
cor(studenti[-270],studenti[-1])
durbinWatsonTest(model,method='resample',reps=1000)
