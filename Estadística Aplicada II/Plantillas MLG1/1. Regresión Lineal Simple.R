########### Importación de la base de datos##########
#############
library(alr4)
#Visualización breve de los datos
#Análisis visual y explotatorio de la base de datos antes del modelo
# Se observan las 10 primeras filas del dataset
head(UBSprices)
#Creo un dataframe con mis variables de interés para realizar el modelo
X<- as.data.frame(cbind(UBSprices$rice2003,UBSprices$rice2009))
colnames(X)<- c("Arroz2003","Arroz2009")
rownames(X)<-rownames(UBSprices)
#Volvemos mas fácil el proceso de llamar las variabes con la función attach
attach(X)
# Evaluamos el coeficiente de correlación lineal de pearson
cor(X)
# Observamos la relación entre las variables
plot(Arroz2009~Arroz2003, pch=19,col='black',xlab="Precio por país en minutos de trabajo para un Kg de Arroz (2003)",
     ylab="Precio por país en minutos de trabajo para un Kg de Arroz (2009)")
#Generamos una variable auxiliar para evaluar la diferencia entre los precios
p<-as.data.frame(X[,2]-X[,1])
# Creamos indices dónde se evidencien los cambios de precios
indices<-which(X[,1]>X[,2])
indices2<-which(X[,1]<X[,2])
indices3<-which(X[,1]==X[,2])
### Más caros
# Cambiar los márgenes del gráfico (el cuarto es el margen derecho)
plot(X[,1],X[,2],xlab="Precio por país en minutos de trabajo para un Kg de Arroz (2003)",
     ylab="Precio por país en minutos de trabajo para un Kg de Arroz (2009)",pch=19, panel.first = grid(),ylim=c(10,100))
#Pintamos con la función point las diferencias de precios
points(X$Arroz2003[indices2],X$Arroz2009[indices2],col='red',pch=19)
points(X$Arroz2003[indices3],X$Arroz2009[indices3],col='black',pch=19)
points(X$Arroz2003[indices],X$Arroz2009[indices],col='blue',pch=19)
legend(x = "topright",legend=c("Aumento","Igual","Disminuyo"),
       col = c("red","black","blue"),pch=c(19,19,19),pt.cex=1,
       box.lwd=0.6,title="Precio 2009vs2003",text.font =15,cex=0.45)
### Evaluamos diferencias mas significativas
colnames(p)<- 'Cambio en el precio'
rownames(p)<- rownames(X)
View(p)
indices4<-c(which(p[,1]>40),which(p[,1]< -10))
text(X$Arroz2003[indices4],X$Arroz2009[indices4],labels=rownames(X)[indices4],cex=0.9,pos=2)
##### Modelo de regresión lineal
model<- lm(Arroz2009~Arroz2003,data=X)
################### Informe Modelo
x.nuevo = data.frame(Arroz2003=seq(min(X[,1]),max(X[,1]),length.out=nrow(X)))
pred.media = predict(model,x.nuevo,interval = 'confidence')
pred.nuev.obs= predict(model,x.nuevo,interval = 'prediction')
plot(X[,1],X[,2],xlab="Precio por país en minutos de trabajo para un Kg de Arroz (2003)",
     ylab="Precio por país en minutos de trabajo para un Kg de Arroz (2009)",pch=19, panel.first = grid(),
     ylim=c(0,100),main='Modelo de regresión lineal')
abline(model)
#Pintamos con la función point las diferencias de precios
points(X$Arroz2003[indices2],X$Arroz2009[indices2],col='red',pch=19)
points(X$Arroz2003[indices3],X$Arroz2009[indices3],col='black',pch=19)
points(X$Arroz2003[indices],X$Arroz2009[indices],col='blue',pch=19)
text(X$Arroz2003[indices4],X$Arroz2009[indices4],labels=rownames(X)[indices4],cex=0.9,pos=2)

lines(x.nuevo$Arroz2003,pred.media[,2],lty=2,col="purple",lwd=2)
lines(x.nuevo$Arroz2003,pred.media[,3],lty=2,col="purple",lwd=2)
lines(x.nuevo$Arroz2003,pred.nuev.obs[,2],lty=3,col="red",lwd=2)
lines(x.nuevo$Arroz2003,pred.nuev.obs[,3],lty=3,col="red",lwd=2)
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       col = c("black","purple","red"),lty = c(1, 2,3),pt.cex=1,
       box.lwd=0.6,text.font =15,cex=0.3)

