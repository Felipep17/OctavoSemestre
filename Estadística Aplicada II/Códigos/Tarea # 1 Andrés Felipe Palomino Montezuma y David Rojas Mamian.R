### Código del Taller número 1
# Andrés Felipe Palomino y David Stiven Rojas ###
#Estadística Aplicada II- Universidad del Valle
# Docente Victor González
#Importación de librerías necesarias
library(car)
library(MASS)
library(xtable)
library(lmtest)
library(readxl)
X<-read_excel('data.xlsx')
attach(X)
X<- cbind(X[,1:30],X[,colnames(X)=='density'])
#Dimensión del dataframe
p<-dim(X)[2]
n<- dim(X)[1]
##############
xtable(head(X[,1:11]))
xtable(head(X[,12:21]))
xtable(head(X[,22:31]))
# Descriptivas
Sd <- apply(X,2,sd)
RESU <- rbind(apply(X,2,summary),Sd)
xtable(RESU[,1:11])
xtable(RESU[,12:21])
xtable(RESU[,22:31])
#Histograma
library(psych)
psych::pairs.panels(X[,c(31,29,28,26,20,3,5)], 
                    method = "pearson",hist.col = "aquamarine1",density = TRUE,ellipses = FALSE) 
#Selección mayor correlación, diagrama de dispersión y modelo
Y<-cor(X)
Y<-Y[,-31]
cor<-c(max(Y[31,]),min(Y[31,]))
cor
which(Y[31,]==cor[1]);which(Y[31,]==cor[2])
  par(mfrow=c(1,1))
plot(X[,29],X[,31],pch=19,col="#FF8C00",panel.first=grid(),xlab="NIR29",
     ylab="densidad del hilo PET",main='Densidad Vs NIR29')
model<- lm(density~NIR29,data=X);abline(model,lwd=2)
summary(model)
# Validación supuestos
#Homocedasticidad
studenti<- studres(model);ajustados<- fitted.values(model)
  plot(ajustados,studenti, ylab='Residuos Estudentizados',
       xlab='Valores Ajustados',pch=19,col="aquamarine4",
       main="Residuos Estudentizados vs Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti~ajustados), col = "red1")
bptest(model,~NIR29+I(NIR29^2),data=X)
# Normalidad
qqPlot(studenti,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",id=F,pch=19)
shapiro.test(studenti)
# Correlación temporal
#Datos desornedados
Z<- as.data.frame(cbind(X[,31],X[,29]))
colnames(Z)<-c('Density','NIR29')
set.seed(100)
ind<-sample(1:nrow(Z),nrow(Z))
Z<- as.data.frame(Z[ind,])
modelprueba<- lm(Density~NIR29,data=Z)
#Datos ordenados
#Validación datos ordenados
par(mfrow=c(1,2))
plot(studres(model),type="b",xlab="Tiempo",
     ylab="Residuos Estudentizados",main="A",pch=19,panel.first=grid())
plot(studres(model)[-length(fitted.values(model))],
     studres(model)[-1],pch=19,panel.first = grid(),col="turquoise3",
     xlab="Residuost-1}",ylab="Residuos{t}",main="B")
abline(lm(studres(model)[-1]~studres(model)[-length(fitted.values(model))]))
durbinWatsonTest(model,method='resample',reps=1000)
par(mfrow=c(1,2))
plot(studres(modelprueba),type="b",xlab="Tiempo",
     ylab="Residuos Estudentizados",main="A",pch=19,panel.first=grid())
plot(studres(modelprueba)[-length(fitted.values(modelprueba))],
     studres(modelprueba)[-1],pch=19,panel.first = grid(),col="turquoise3",
     xlab="Residuost-1}",ylab="Residuos{t}",main="B")
abline(lm(studres(modelprueba)[-1]~studres(modelprueba)
          [-length(fitted.values(modelprueba))]))
durbinWatsonTest(modelprueba,method='resample',reps=1000)
# Transformación de varianza
res.estu <- residuals(model)
varianza<- lm(abs(res.estu)~NIR29,data=X)
w = 1/(fitted.values(varianza))^2
model.ponderados<- lm(density~NIR29,data=X,weights = w)
# Validación MCP SUPUESTOS
studenti.ponderados<- residuals(model.ponderados)*sqrt(w)
ajustados.ponderados<- fitted.values(model.ponderados)
par(mfrow=c(1,2))
plot(ajustados.ponderados,studenti.ponderados, ylab='Residuos Ponderados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",
     main="A")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.ponderados~ajustados.ponderados), col = "red1")
qqPlot(studenti.ponderados,main="B", ylab="Residuos Ponderados",
       xlab="Cuantile Teóricos",id=F,pch=19)
shapiro.test(studenti.ponderados)
summary(model.ponderados)
#Anova
anova(model.ponderados)
#Residuos ponderados
studenti.ponderados<- residuals(model.ponderados)*sqrt(w)
# Identificación puntos atípicos en influyentes
res.ponderados<- residuals(model.ponderados)*sqrt(w)
library(car)
par(mfrow=c(1,1))
p<- length(coefficients(model.ponderados))
n<- nrow(X)
hii.c<- 2*p/n
hii<- hatvalues(model.ponderados)
hii.ind<- hii[hii>hii.c]
n<- length(residuals(model.ponderados))
p<- length(coefficients(model.ponderados))
hii.c<-2*(p/n)
plot(hii,res.ponderados,pch=19,xlab="Valores de la diagonal de la matriz hat",
       ylab=" Residuos Ponderados",ylim=c(-3,3),xlim=c(0,0.3),panel.first=grid())
abline(h=c(1,0,-1)*2,lty=2,v=hii.c)
# DFBETAS Y D COOK
par(mfrow=c(1,2))
ck<- cooks.distance(model.ponderados)
plot(ck,ylab="Distancia de Coock",pch=19,ylim=c(min(ck),max(ck)+0.1),
     panel.first=grid())
ck.c<- 4/n
abline(h=ck.c,lty=2)
indices<- (1:nrow(X))[ck>ck.c]
ck<- ck[ck>ck.c]
points(indices,ck,col="red",pch=19)
text(indices,ck,labels=rownames(X)[indices],pos=3,cex=0.6)


DFBETAS = dfbetas(model.ponderados)
plot(DFBETAS[,2],ylab=quote('DFBETA'~(beta[1])),xlab="Indices",
     pch=19,ylim=c(-0.4,0.5),xlim=c(0,150),panel.first=grid())
ind = (1:nrow(X))[abs(DFBETAS[,2]) > 2/sqrt(nrow(X))]
dfb = DFBETAS[abs(DFBETAS[,2]) > 2/sqrt(nrow(X)) ,2]
abline(h=c(1,-1)*2/sqrt(nrow(X)))
text(ind,dfb,rownames(X)[abs(DFBETAS[,2]) > 2/sqrt(nrow(X))],
     pos=c(1,3,1,4,3,2,1,4,3,4),
     cex=0.8)
points(ind,dfb,col="red",pch=19)
# COVRATIO DDFITS
par(mfrow=c(1,2))
DFFITS = dffits(model.ponderados)
plot(DFFITS,xlab="Indices",pch=19,ylim=c(-1.3,1.3),panel.first=grid())
abline(h=c(-1,1)*2*sqrt(p/n))
ind = (1:nrow(X))[abs(DFFITS) > 2*sqrt(p/n)]
dfb = DFFITS[abs(DFFITS) > 2*sqrt(p/n)]
text(ind,dfb,rownames(X)[abs(DFFITS) > 2*sqrt(p/n)],pos=2)
points(ind,dfb,col="purple4",pch=19)

COVR = covratio(model.ponderados)
plot(COVR,pch=19,ylab="Covratio",xlab="Indice",panel.first=grid())
abline(h=1+c(-1,1)*3*(p/n))
covr = COVR[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
ind = (1:nrow(X))[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
text(ind,covr,rownames(X)[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n)],pos=4)
points(ind,covr,col="purple4",pch=19)
# Intervalos de confianza para los beta
confint(model.ponderados)
# Gráfico modelo ajustado
x.nuevo = data.frame(NIR29=seq(min(X[,29]),max(X[,29]),length.out=100))

#Predicción del intervalo de confianza
par(mfrow=c(1,1))
pred.media = predict(model.ponderados,x.nuevo,interval = "confidence")
plot(X$NIR29,X$density,pch=19,col="#FF8C00",panel.first=grid(),xlab="NIR29",
       ylab="densidad del hilo PET",main='Densidad Vs NIR29')
#Gráficas de las lineas
lines(x.nuevo[,1],pred.media[,2],lty=2,lwd=3)
lines(x.nuevo[,1],pred.media[,3],lty=2,lwd=3)

abline(model.ponderados)
legend(x = "topright",legend=c("Modelo","Intervalo de confianza 95%"),
       lty = c(1, 2,3),pt.cex=1.5,
       box.lwd=0.6,text.font =15,cex=0.8) #Caja de enunciados
# Predicción
ejemplo <- data.frame(NIR29=1.3)
pred.media = predict(model.ponderados,ejemplo,interval = "confidence")
pred.media
# Modelo escalonado
Z <- data.frame(scale(X))
xtable(head(Z[,1:11]))
xtable(head(Z[,12:21]))
xtable(head(Z[,22:31]))
modelz<- lm(density~NIR29,data=Z)
summary(modelz)
# Gráfico y modelo
par(mfrow=c(1,1))
plot(Z[,29],Z[,31],ylab='ZDensidad',
     xlab=' Z NIR 29',panel.first=grid(),col="#FF8C00",pch=19)
abline(modelz)
# Supuestos escalonado
library(MASS)
library(lmtest)
studenti.<- studres(modelz);ajustados.<- fitted.values(modelz)
par(mfrow=c(1,2))
plot(ajustados.,studenti., ylab='Res Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",
     main="Res Estudentizados vs Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.~ajustados.), col = "red1")
qqPlot(studenti.,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",id=F,
pch=19,main="QQPLOT")
bptest(modelz,~NIR29+I(NIR29^2),data=Z)
shapiro.test(studenti.)
# MCP estandarizado
res.estuz <- residuals(modelz)
varianzaz<- lm(abs(res.estuz)~NIR29,data=Z)
wz= 1/(fitted.values(varianzaz))^2
model.ponderadosz<- lm(density~NIR29,data=Z,weights = w)
# Validación MCP
studenti.ponderadosz<- residuals(model.ponderadosz)*sqrt(wz)
ajustados.ponderadosz<- fitted.values(model.ponderadosz)
par(mfrow=c(1,2))
plot(ajustados.ponderadosz,studenti.ponderadosz, ylab='Residuos Ponderados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",
     main="A")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.ponderadosz~ajustados.ponderadosz), col = "red1")
qqPlot(studenti.ponderados,main="B", ylab="Residuos Ponderados",
       xlab="Cuantiles Teóricos",id=F,pch=19)
shapiro.test(studenti.ponderados)
summary(model.ponderadosz)
# Atípicos e influyentes estandarizado
par(mfrow=c(1,4))
ck<- cooks.distance(model.ponderadosz)
plot(ck,ylab="Distancia de Coock",pch=19,ylim=c(min(ck),max(ck)+0.1),
     panel.first=grid())
ck.c<- 4/n
abline(h=ck.c,lty=2)
indices<- (1:nrow(Z))[ck>ck.c]
ck<- ck[ck>ck.c]
points(indices,ck,col="red",pch=19)
text(indices,ck,labels=rownames(X)[indices],pos=3,cex=0.6)


DFBETAS = dfbetas(model.ponderadosz)
plot(DFBETAS[,2],ylab=quote('DFBETA'~(beta[1])),xlab="Indices",
     pch=19,ylim=c(-0.4,0.5),xlim=c(0,150),panel.first=grid())
ind = (1:nrow(Z))[abs(DFBETAS[,2]) > 2/sqrt(nrow(Z))]
dfb = DFBETAS[abs(DFBETAS[,2]) > 2/sqrt(nrow(Z)) ,2]
abline(h=c(1,-1)*2/sqrt(nrow(Z)))
text(ind,dfb,rownames(Z)[abs(DFBETAS[,2]) > 2/sqrt(nrow(Z))],
     pos=c(1,3,1,4,3,2,1,4,3,4),
     cex=0.8)
points(ind,dfb,col="red",pch=19)


DFFITS = dffits(model.ponderadosz)
plot(DFFITS,xlab="Indices",pch=19,ylim=c(-1.3,1.3),panel.first=grid())
abline(h=c(-1,1)*2*sqrt(p/n))
ind = (1:nrow(X))[abs(DFFITS) > 2*sqrt(p/n)]
dfb = DFFITS[abs(DFFITS) > 2*sqrt(p/n)]
text(ind,dfb,rownames(X)[abs(DFFITS) > 2*sqrt(p/n)],pos=2)
points(ind,dfb,col="purple4",pch=19)

COVR = covratio(model.ponderadosz)
plot(COVR,pch=19,ylab="Covratio",xlab="Indice",panel.first=grid())
abline(h=1+c(-1,1)*3*(p/n))
covr = COVR[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
ind = (1:nrow(X))[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
text(ind,covr,rownames(X)[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n)],pos=4)
points(ind,covr,col="purple4",pch=19)
# Intervalos
confint(model.ponderadosz)
# Gráfico
x.nuevo = data.frame(NIR29=seq(min(Z[,29]),max(Z[,29]),length.out=100))
#Predicción del intervalo de confianza
pred.media = predict(model.ponderadosz,x.nuevo,interval = "confidence")
par(mfrow=c(1,1))
plot(Z$NIR29,Z$density,pch=19,col="#FF8C00",panel.first=grid(),xlab="NIR29",
       ylab="densidad del hilo PET",main='Densidad Vs NIR29 estandarizado')
#Gráficas de las lineas
lines(x.nuevo[,1],pred.media[,2],lty=2,lwd=3)
lines(x.nuevo[,1],pred.media[,3],lty=2,lwd=3)

abline(model.ponderadosz)
legend(x = "topright",legend=c("Modelo","Intervalo de confianza 95%"),
       lty = c(1, 2,3),pt.cex=1.5,
       box.lwd=0.6,text.font =15,cex=0.8) #Caja de enunciados
###### Prediccio
ejemploz <- data.frame(NIR29=1)
pred.media = predict(model.ponderadosz,ejemploz,interval = "confidence")
pred.media
# Polinómico
model.<- lm(density~NIR29+I(NIR29^2),data=X)
summary(model.)
# Gráfico
plot(X[,29],X[,31],pch=19,col="#FF8C00",panel.first=grid(),xlab="NIR29",
     ylab="densidad del hilo PET",main='Densidad Vs NIR29')
#Gráficas de las lineas
lines(spline(X[,29],fitted.values(model.)),lty=2,lwd=3)
legend(x = "topright",legend=c("Modelo polinómico df=2"),
       lty = c(2),lwd=2,pt.cex=1.5,
       box.lwd=0.6,text.font =15,cex=0.8) #Caja de enunciados
#supuestos
studenti.z<- studres(model.)
ajustados.z<- fitted.values(model.)
<<fsada, fig.height=4, echo=T>>=
  par(mfrow=c(1,2))
plot(ajustados.z,studenti.z, ylab='Residuos Ponderados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",
     main="A")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.z~ajustados.z), col = "red1")
qqPlot(studenti.z,main="B", ylab="Residuos Ponderados",
       xlab="Cuantiles Teóricos",id=F,pch=19)
shapiro.test(studenti.z)
bptest(model.,~NIR29+I(NIR29^2),data=Z)

