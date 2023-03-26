############ Simulación de la primera tarea
#################### Exploración de una serie temporal estacionaria########
X<- rnorm(1000) #Vector de 1000 variables independientes e identicamente distribuidas
#Gráfico de la trayectoría
par(mfrow=c(1,1))
plot.ts(X,ylab='',xlab='',main='Serie Temporal Estacionaria',panel.first=grid())
abline(h=mean(X),lty=2,lwd=2,col='aquamarine')
#Gráfio de la trayectoria de una serie temporal no iid con media diferente y varianza constante
n<- 1:100
mu<- 9-5*n
X<-rnorm(100,mu)
plot.ts(X,lwd=2,xlab='',ylab='',panel.first=grid(),main='Trayectoría serie no iid con función lineal negativa para la media')
abline(h=mean(X),lwd=2,lty=2)
#Serie temporal con media no constante y varianza no constante
TT<-rnorm(100,9-5*n+rnorm(100),abs(2*mu)+n)
plot(TT,type='l',ylab='',xlab='')
abline(h=mean(TT),lwd=2,lty=2)
#Exploraciónd e una serie temporal con diferentes distribuciones asociadadas a cada observación
X<- c(rbinom(100,10,0.5),rnorm(100,mu,abs(2*mu)),rgamma(100,abs(rnorm(100))+2,2),rpois(100,10),rweibull(100,abs(rnorm(100))+2,5))
Y<- sample(X,length(X))
plot.ts(Y,xlab='',ylab='',panel.first=grid())
abline(h=mean(Y),lwd=2,lty=2)
##########
library(forecast)
library(timeSeries)
library(tseries)
adf.test(X)

