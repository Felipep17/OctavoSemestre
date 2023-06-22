setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Series de Tiempo y Pronóstico/Trabajo final")
require("sf")
require("spdep")
require("dplyr")
library(readr)
library(ggspatial)
library(Metrics)
require("tidyverse")
require("here")
require("spatstat")
library(ggplot2)
require(mapdata)
require(maps)
require(ggrepel)
library(tseries)
library(MASS)
library(forecast)
library(readxl)
library(car)
library(zoo)
library(lubridate)
library(dygraphs)
library(TSA)
library(anomalize)
library(tidyverse)
library(pracma)
library(readr)
library(lubridate)
library(zoo)
library(forecast)
library(MASS)
library(tseries)
lambda<- function(model,a,b){
  par(mfrow=c(1,1))
  box.cox<-boxcox(model,lambda=seq(a,b,length.out = 10000),
                  ylab='Log-verosimilitud')
  bc<-round(box.cox$x[box.cox$y ==max(box.cox$y)],2)
  print(bc)
}
#Importación base de datos
MLTollsStackOverflow <- read_csv("MLTollsStackOverflow.csv")
X<- MLTollsStackOverflow
fecha_inicio <- as.Date("2009-01-09")
secuencia_meses <- seq(from = fecha_inicio, by = "month",length.out=132)
#Gráfico serie temporal
ts<- zoo(X$Pygtk,order.by=secuencia_meses)
ts1<- ts(X$Pygtk,freq=12)
dygraphs::dygraph(ts,main="Serie temporal por meses para StackOverFlow en Pygtk")
lambda(lm(X$Pygtk~1),-3,3)
plot(decompose(ts1),col="blue4")
tslog<- zoo(log(X$Pygtk),order.by=secuencia_meses)
dygraphs::dygraph(tslog,main="Serie temporal transformada por meses para StackOverFlow en Pygtk")
dygraphs::dygraph(diff(tslog),main="Serie temporal transformada y diferenciada por meses para StackOverFlow en Pygtk")
plot(decompose(ts(log(X$Pygtk),freq=12,start=c(09,01,2016))))
plot.ts(ts(log(X$Pygtk),freq=12,start=c(09,01,2016)))
plot(decompose(diff((ts(log(X$Pygtk),freq=12,start=c(09,01,2016))))))
##Analizar ACF y PACF
eacf(ts(diff(log(X$Pygtk)),freq=12,start=c(01,2009)))
tsdisplay(ts(diff(log(X$Pygtk)),freq=12,start=c(09,01,2016)),lwd=1,main='% TIB')
layout_matriz <- matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE)
# Establecer el layout
u<- acf(diff(log(ts1)))
w<- pacf(diff(log(ts1)))
layout(layout_matriz)
# Crear el gráfico
library(TSA)
auto.arima(ts(X$Pygtk,freq=12,start=c(09,01,2016)))
u$acf[u$acf>0]<-u$acf[u$acf>0]-0.05
u$acf[u$acf<0]<-u$acf[u$acf<0]+0.05
plot(diff(log(ts1)),xlab="Time",ylab="Serie diferenciada y transformada",main="Log Diff Count Pytgk",type="o",pch=19)
plot(u$acf,type="h",ylim=c(-0.5,0.5),ylab="ACF",xlab="Lag")
abline(h=c(0,-0.2,0.2),col=c("black","blue4","blue4"),lty=c(1,2,2))
pacf(diff(log(X$Pygtk)),main="")
modelofinal<- Arima(log(X$Pygtk),order = c(0,1,1))
coeficientes <- coef(modelofinal)
matriz_cov <- vcov(modelofinal)
grados_libertad <- length(X$Pygtk) - length(coeficientes)
p_valores <- 2 * (1 - pt(abs(coeficientes)/sqrt(diag(matriz_cov)), df = grados_libertad))
resultados <- data.frame(Coeficientes = coeficientes, P_valores = p_valores)
print(resultados)
lines(exp(fitted.values(modelofinal)),col="blue")
car::qqPlot(residuals(modelofinal))
checkresiduals(modelofinal)
res_est_w1=residuals(modelofinal)/(modelofinal$sigma2^.5)
y<-c()
x<-1:20 
for(i in 1:length(x)){
  y[i]<-Box.test(res_est_w1, lag = x[i], type = c("Ljung-Box"), fitdf = 0)$p.value
}
x11()

u1<- acf(residuals(modelofinal))
u2<- pacf(residuals(modelofinal))
u1$acf[u1$acf>0]<-u1$acf[u1$acf>0]-0.045
u1$acf[u1$acf<0]<-u1$acf[u1$acf<0]
u2$acf[u2$acf>0]<-u2$acf[u2$acf>0]-0.045
u2$acf[u2$acf<0]<-u2$acf[u2$acf<0]
layout_matriz <- matrix(c(1, 1, 2,3,4,5), nrow = 3, ncol = 2, byrow = T)
layout(layout_matriz)
plot(res_est_w1,type='o',pch=19,ylab="Standarized Residuals",xlab="Time",main="Validación de supuestos para el modelo ARIMA(0,1,1)")
plot(u1$acf,type="h",ylim=c(-0.25,0.25),main="ACF of Residuals",ylab="ACF",xlab="Lag")
abline(h=c(0,-0.2,0.2),col=c("black","blue4","blue4"),lty=c(1,2,2))
plot(u2$acf,type="h",ylim=c(-0.25,0.25),main="PACF of Residuals",ylab="PACF",xlab="Lag")
abline(h=c(0,-0.2,0.2),col=c("black","blue4","blue4"),lty=c(1,2,2))
car::qqPlot(res_est_w1,pch=19,xlab="Theoretical Quantile",ylab="Sample Quantiles",main="Normal Q-Q Plot to Residuals")
plot(y,ylim=c(0,1),ylab="P-Value",xlab="Lag",pch=19,main="p values for Ljung-Box statistic")
abline(h=c(0.05),lwd=2,lty=2,col="blue4")
library(feasts)
h<-tsdiag(modelofinal)
fBasics::jarqueberaTest(modelofinal$residuals)             # librer?a fBasics
fBasics::normalTest(modelofinal$residuals, method=("jb"))
library(tseries)
white.test(modelofinal$residuals, lag=2)
h<-astsa::sarima(xdata = log(X$Pygtk), p = 0, d = 1, q = 1)
#gráfico
`ARIMA(0,1,1)`<- zoo(fitted.values(modelofinal),order.by=secuencia_meses)
`Serie Transformada`<- zoo(log(X$Pygtk),order.by=secuencia_meses)
Series<- cbind()
"ARIMA(0,1,1)"
dygraph(cbind(`Serie Transformada`,`ARIMA(0,1,1)`),main="Serie temporal transformada y ajuste del modelo")
ts1<- ts(log(X$Pygtk),freq=12)
plot(as.numeric(tslog),type="l")
lines(fitted.values(modelofinal),lwd=2,col="red3")
modelauto<-forecast::auto.arima(ts1)
u<-forecast::forecast(modelauto,5)
par(mfrow=c(1,1))
plot(u, main = ""
     , ylab = "Log Número de consultas para Pytgk", xlab= "Mes")
pre=forecast::forecast(modelauto,2)
pre_final=exp(pre$mean)
arimafore <- forecast::forecast(modelauto, h = 2)
plot(arimafore,las=2)
#exponencial de los pronosticos
#media
exp(pre[["mean"]])
#intervalos de confianza
exp(pre[["lower"]])
exp(pre[["upper"]])
library(lmtest)
coeftest(modelofinal)

#la raices fuera del circulo unitario, lo cual implica estacionariedad e invertibilidad
library(UnitCircle)
UnitCircle::uc.check(c(1, -coef(modelofinal)[1]))
