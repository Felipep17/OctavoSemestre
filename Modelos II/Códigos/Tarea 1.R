#Primer punto
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Modelos II/Bases de datos")
library(readr)
library(car)
library(latex2exp)
library(xtable)
X <- read_csv("ChemReact.csv") #Importo la base de datos
par(mfrow=c(1,2))
attach(X)
vif(model.)
#Análisis exploratorio de datos
plot(Yield~Time,pch=19,col='red1',ylab='Rendimiento',xlab='Tiempo')
plot(Yield~Temp,pch=19,col='aquamarine1',ylab='Rendimiento',xlab='Temperatura')
#Modelo de regresión sin las variables centradas
model.<- lm(Yield~Temp+Time+I(Time^2)+I(Temp^2)+Time*Temp,data=X)
summary(model)
vif(model.) #Multicolinealdiad
#Variables centradas
X$Temp.c<- Temp-mean(Temp)
X$Time.c<- Time-mean(Time)
#Modelo con las variables centradas
model<- lm(Yield~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2)+Time.c*Temp.c,data=X)
y<- vif(model.)
l<-vif(model)
viff<-as.data.frame(rbind(as.numeric(y),as.numeric(l)))
colnames(viff) <- colnames(X)
rownames(viff)<- c('Sin Centrar','Centrado')
xtable(viff)
# Validación de supuestos
#Evaluación de heterocedasticidad
par(mfrow=c(1,2))
library(MASS)
studenti<- studres(model)
ajustados<- fitted.values(model)
plot(ajustados,studenti, panel.first=grid(),ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="A")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti~ajustados), col = "red1")
library(lmtest)
bptest(model,~Temp+Time+I(Time^2)+I(Temp^2)+Time*Temp,data=X)
#Residuos parciales para evaluar Linealidad y asegurar el supuesto de que la esperanza de los errores es igual a 0
shapiro.test(studenti)
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main='B')
#Transformaciones
#Correción de normalidad
#Calculo del lambda más óptimo
box.cox<- boxcox(model,lambda=seq(-4,4,length.out = 1000),
                 ylab='Log-verosimilitud')
#Selección del Lambda
bc<-round(box.cox$x[box.cox$y ==max(box.cox$y)],2)
#Ajuste del modelo
model.box<- lm(I(Yield^bc)~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2)+Time.c*Temp.c,data=X)
#Resumen del modelo
summary(model.box)
#Validación de supuestos del modelo con BOX-COX
studenti.box<- studres(model.box)
ajustados.box<- model.box$fitted.values
par(mfrow=c(1,2))
plot(ajustados.box,studenti.box, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="A")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.box~ajustados.box), col = "red1")
bptest(model.box,~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2)+Time.c*Temp.c,data=X)
qqPlot(residuals(model.box),xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main="B")
shapiro.test(residuals(model.box))
################### MCP##################
#Estimación varianza
res.mcp<- residuals(model)
varianza<- lm(abs(res.mcp)~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2)+Time.c*Temp.c,data=X)
w = 1/varianza$fitted.values^2
model.ponderados<- lm(Yield~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2)+Time.c*Temp.c,weights = w,data=X)
#Validación Supuestos
par(mfrow=c(1,2))
res.ponderados<- residuals(model.ponderados)*sqrt(w)
plot(fitted.values(model.ponderados),res.ponderados,
     xlab='valores ajustados',ylab='residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4",main='A')
lines(lowess(res.ponderados~fitted.values(model.ponderados)),col=2,lty=2,lwd=4)
abline(h=0,lty=2,lwd=2)
qqPlot(res.ponderados,pch=19,ylab='Residuos Ponderados',xlab='Cuantiles Teóricos',main='B')
shapiro.test(res.ponderados)
# Omisión de la variable irrelevante
model.in<- lm(Yield~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2),data=X)
anova(model,model.in) #Significativamente
############# Supuestos en general###############
par(mfrow=c(2,3))
############## Varianza
#Modelo centrado sin transformaciones
studenti<- studres(model)
ajustados<- fitted.values(model)
plot(ajustados,studenti, panel.first=grid(),ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',cex=1.5,pch=19,col="aquamarine4",main="Modelo Centrado",cex.main = 2,   # Tamaño del título
     cex.sub = 1.5,  # Tamaño del subtítulo
     cex.lab = 1.5,    # Tamaño de las etiquetas de los ejes X e Y
     cex.axis = 1.5)
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti~ajustados), col = "red1")
############ BOX-COX
studenti.box<- studres(model.box)
ajustados.box<- model.box$fitted.values
plot(ajustados.box,studenti.box,cex=1.5, panel.first=grid(), ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main=TeX("$(Modelo Box-Cox)  \\lambda=1.333$"),cex.main = 2,   # Tamaño del título
     cex.sub = 2,  # Tamaño del subtítulo
     cex.lab = 2,    # Tamaño de las etiquetas de los ejes X e Y
     cex.axis = 1.5)
abline(h=0,lty=2,lwd=2)
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.box~ajustados.box), col = "red1")
# Model Ponderados
plot(fitted.values(model.ponderados),cex.main = 2,cex=1.5,res.ponderados,
     xlab='valores ajustados',   # Tamaño del título
     cex.sub = 1.5,  # Tamaño del subtítulo
     cex.lab = 1.5,    # Tamaño de las etiquetas de los ejes X e Y
     cex.axis = 1.5,ylab='residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4",main='Minimos Cuadrados Ponderados')
lines(lowess(res.ponderados~fitted.values(model.ponderados)),lty=1,lwd=1,col='red1')
abline(h=0)
##################################### NORMALIDAD
#Centrado
qqnorm(residuals(model),panel.firs=grid(),xlab="Cuantiles Teóricos",cex.main = 2,   # Tamaño del título
       cex.sub = 1.5,  # Tamaño del subtítulo
       cex.lab = 1.5,    # Tamaño de las etiquetas de los ejes X e Y
       cex.axis = 1.5,cex=1.5,ylab="Residuos Estudentizados",pch=19,main='')
qqline(residuals(model))
# Box-Cox
qqnorm(residuals(model.box),cex.main = 2, panel.firs=grid(),  # Tamaño del título
       cex.sub = 1.5,  # Tamaño del subtítulo
       cex.lab = 1.5,    # Tamaño de las etiquetas de los ejes X e Y
       cex.axis = 1.5,cex=1.5,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main="")
qqline(residuals(model.box))
#Ponderados
qqnorm(res.ponderados,panel.first=grid()  , # Tamaño del título
       cex.sub = 1.5,  # Tamaño del subtítulo
       cex.lab = 1.5,    # Tamaño de las etiquetas de los ejes X e Y
       cex.axis = 1.5,pch=19,ylab='Residuos Ponderados',xlab='Cuantiles Teóricos',main='',cex=1.5)
qqline(res.ponderados)
#Tabla de resumen de estadísticos
shapiro.test(studenti)
shapiro.test(studenti.box)
shapiro.test(res.ponderados)
bptest(model.box,~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2)+Time.c*Temp.c,data=X)
bptest(model,~Temp.c+Time.c+I(Time.c^2)+I(Temp.c^2)+Time.c*Temp.c,data=X)
Hipo<-matrix(0,4,3)
Hipo[,1]<- c('Modelo/Valores P','Modelo Centrado','Modelo Box-Cox', 'Modelo MCP')

Hipo[,2]<- c('Shapiro Wilk(p-value)',0.006628,0.005993,0.0002527)
Hipo[,3]<- c('Breusch Pagan(p-value)',0.558,0.6521,'No Aplica')
Hipo<-as.data.frame(Hipo)
colnames(Hipo)<- Hipo[1,]
rownames(Hipo)<- Hipo[,1]
Hipo<- Hipo[,-1]
Hipo<- Hipo[-1,]
Hipo
xtable(Hipo)
#Residuos parciales para evaluar Linealidad y asegurar el supuesto de que la esperanza de los errores es igual a 0
shapiro.test(studenti)
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main='B')
# No se logra evidenciar cambios con las trasnformaciones por lo cuál
# Trabajaremos con el modelo sin la interacción y en general
summary(model.in)
predict(model.in,newdata=data.frame(Temp.c=1.54,Time.c=1.78),interval='confidence') # Valor que maximiza
summary(model.in)
############################# Curva modelo centrado
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
objr<-lm(z ~ x+y+I(x^2)+I(y^2)+x*y)
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
                      facets = NA, fit = fitpoints), main = "",xlab='Tiempo ',ylab='Temperatura',col = c('red4','red3','red2','red1','orange4','orange3','orange2','orange1','yellow4','yellow3','yellow2','yellow'))
summary(model.in)
#Gráfico dinámico
plotrgl()
############################# Curva modelo sin centrar
z<-X$Yield
y<-X$Temp
x<-X$Time
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
                      facets = NA, fit = fitpoints), main = '',ylab='Temperatura ',xlab='Tiempo',col = c('red4','red3','red2','red1','orange4','orange3','orange2','orange1','yellow4','yellow3','yellow2','yellow'))
#Gráfico dinámico
plotrgl()
# Gráfico de los contornos
X<- as.data.frame(X)
X1 = seq(min(X[,2]), max(X[,2]), length.out = 50)
X2 = seq(min(X[,1]), max(X[,1]), length= 50)
y <- outer(X= X1, Y = X2, FUN = function(x, y) {
  predict(model.in, newdata = data.frame(Temp.c = x-mean(X[,2]), Time.c = y-mean(X[,1])))
})
par(mfrow=c(1,1))
contour(X1, X2, y,xlab='Temperatura',
        ylab='Tiempo',col=hcl.colors(10,palette='viridis'))
library(MASS)
z <- kde2d(X1, X2, n = 50)
filled.contour(X1,X2,y,color=topo.colors,xlab='Temperatura',ylab='Tiempo')
filled.contour(X1,X2,y,xlab='Temperatura',ylab='Tiempo', plot.axes = {
  axis(1)
  axis(2)
  contour(y, add = TRUE, lwd = 2)
}
)

#Segundo Punto
#Importó la librería e inicializó un vector con la información
library(alr4)
X<- oldfaith
#Evaluó el tipo de relación entre las variables:
par(mfrow=c(1,2))
#Estimo el modelo
plot(X$Duration,X$Interval,pch=19,col="#FF8C00",panel.first=grid(),xlab="Duración en segundos",ylab="Intervalo en minutos",
     main='',cex.lab=2,cex.axis=1.5,cex=1.5)
points(X$Duration[X>180],X$Interval[X$Duration>180],col='#8B4500',pch=19,cex=1.5)
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
#Caja de enunciados
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       lty = c(1, 2,3),pt.cex=1.5,
       box.lwd=0.6,text.font =15,cex=0.8)
#Gráfica de R
#Gráfica del modelo
abline(model,lwd=2)
#####################
X$Duration.x = X$Duration - 180
X$Duration.x[X$Duration < 180] = 0
mod.segmento = lm(Interval~.,data=X)
summary(mod.segmento)
xtable(mod.segmento)
xtable(anova(model,mod.segmento))
AIC(model)
AIC(mod.segmento)
############
#Estimo el modelo
b.lotes<- coefficients(mod.segmento)
#

plot(X$Duration,X$Interval,pch=19,col="#FF8C00",panel.first=grid(),xlab="Duración en segundos",ylab="Intervalo en minutos",
     main='',cex.lab=2,cex.axis=1.5,cex=1.5)
points(X$Duration[X>180],X$Interval[X$Duration>180],col='#8B4500',pch=19,cex=2)
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
X[,1][X>180]
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
       lty = c(1, 2,3),pt.cex=1.5,
       box.lwd=0.6,text.font =15,cex=0.8)
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
BIC(model)
BIC(mod.segmento)
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
plot(studenti[-1],studenti[-nrow(X)],pch=19,panel.first = grid(),col="turquoise3",xlab="Residuos(t-1)",ylab="Residuos(t)",main="B")
abline(lm(studenti[-1]~studenti[-nrow(X)]))
cor(studenti[-270],studenti[-1])
durbinWatsonTest(model,method='resample',reps=1000)

