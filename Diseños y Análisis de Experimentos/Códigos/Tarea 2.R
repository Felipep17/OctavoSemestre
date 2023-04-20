X<- as.numeric(c(25.1,17.2,26.4,16.1,22.2,15.9,40.2,35.2,32,36.5,43.3,37.1,18.3,22.6,25.9,15,11.4,23.7,28.6,28,33.2,31.7,30.3,27.6))
Y<- as.character(c("V1","V1","V1","V1","V1",'V1',"V2","V2","V2","V2","V2",'V2',"V3","V3","V3","V3","V3",'V3',"V4","V4","V4","V4","V4",'V4'))
length(Y)
Y<-factor(Y)
U<- as.data.frame(cbind(X,Y))
colnames(U)<- c("Rendimiento Melon","Variedad")
boxplot(U[,1],panel.first=grid(),xlab=paste(colnames(U)[2]),ylab=paste(colnames(U)[1]))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(U[,1],col=c('aquamarine1','yellow','orange','red'),pch=19,add=T)
stripchart(U[,1], method = "jitter", pch = 19, add = TRUE, col = "blue")
legend(x = "topright",legend=c("1","2","3","4"),
       col=c('aquamarine1','yellow','orange', 'red'),pt.cex=1,pch=15,title='Variedad',
       box.lwd=1,text.font =20,cex=0.8)
?stripchart
mean(U$`Rendimiento Melon`[U$Variedad=='1'])
mean(U$`Rendimiento Melon`[U$Variedad=='2'])
mean(U$`Rendimiento Melon`[U$Variedad=='3'])
mean(U$`Rendimiento Melon`[U$Variedad=='4'])
mean(U$`Rendimiento Melon`[U$Variedad=='1'])-mean(U$`Rendimiento Melon`)
mean(U$`Rendimiento Melon`[U$Variedad=='2'])-mean(U$`Rendimiento Melon`)
mean(U$`Rendimiento Melon`)-mean(U$`Rendimiento Melon`[U$Variedad=='3'])
mean(U$`Rendimiento Melon`[U$Variedad=='4'])-mean(U$`Rendimiento Melon`)
#########
Sum<<- NULL
Sum[1]<-sum(U$`Rendimiento Melon`[U$Variedad=='1'])^2/6
Sum[2]<-sum(U$`Rendimiento Melon`[U$Variedad=='2'])^2/6
Sum[3]<-sum(U$`Rendimiento Melon`[U$Variedad=='3'])^2/6
Sum[4]<-sum(U$`Rendimiento Melon`[U$Variedad=='4'])^2/6
sum(Sum)-sum(U$`Rendimiento Melon`)^2/24
Anova<- matrix(0,3,5)
colnames(Anova)<- c('Fuente de Variabilidad','Grados de Libertad','Suma de Cuadrados','Cuadrado Medio', "F0")
Anova<-as.data.frame(Anova)
View(Anova)
Anova[1,]<-c("t",3 , 1290.301, (1290.301)/3, 430.100333333333/((sum(U$`Rendimiento Melon`^2)-18544.15)/20))
Anova[2,]<-c("Error",21 , sum(U$`Rendimiento Melon`^2)-18544.15, (sum(U$`Rendimiento Melon`^2)-18544.15)/20, 0)
Anova[3,]<-c(23,1290.301+sum(U$`Rendimiento Melon`^2)-18544.15,0,0,0)
library(xtable)
xtable(Anova)
options(scipen=999)
1-pf(23.36,3,20)
# Vertical box plot
boxplot(U[,1]~U[,2],ylab='Rendimiento del Melón',xlab='Variedad' ,col = c('aquamarine1','yellow','orange','red'))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(U[,1]~U[,2], col = c('aquamarine1','yellow','orange','red'),add=T)
# Points
stripchart(U[,1]~U[,2],              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 'black',           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   
######### Anova con R
V1<-c(rep(1,6),rep(0,18))
V2<-c(rep(0,6),rep(1,6),rep(0,12))
V3<-c(rep(0,6),rep(0,6),rep(1,6),rep(0,6))
V4<- c(rep(0,18),rep(1,6))
X<- cbind(V1,V2,V3,V4)
Y<- as.numeric(c(25.1,17.2,26.4,16.1,22.2,15.9,40.2,35.2,32,36.5,43.3,37.1,18.3,22.6,25.9,15,11.4,23.7,28.6,28,33.2,31.7,30.3,27.6))
model<- aov(Y~X)
library(ggfortify)
autoplot(lm(Y~X))
model$coefficients
summary(model)
Z<- cbind(U[1:6,1],U[7:12,1],U[13:18,1],U[19:24,1])
colnames(Z)<- c('V1','V2','V3','V4')
