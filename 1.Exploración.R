### Estacionariedad
library(tseries)
library(forecast)
n<- 1:100
mu<- 9-5*n
abs(rnorm(100))
X<- c(rbinom(100,10,0.5),rnorm(100,mu,abs(2*mu)),rgamma(100,abs(rnorm(100))+2,2),rpois(100,10),rweibull(100,abs(rnorm(100))+2,5))
Y<- sample(X,length(X))
TT<-rnorm(100,9-5*n,abs(2*mu)+n)
plot(TT,type='l')
plot.ts(Y)
adf.test(Y)
adf.test(TT)

