X<- as.numeric(c(25.1,17.2,26.4,16.1,22.2,15.9,40.2,35.2,32,36.5,43.3,37.1,18.3,22.6,25.9,15,11.4,23.7,28.6,28,33.2,31.7,30.3,27.6))
Y<- as.character(c("V1","V1","V1","V1","V1",'V1',"V2","V2","V2","V2","V2",'V2',"V3","V3","V3","V3","V3",'V3',"V4","V4","V4","V4","V4",'V4'))
length(Y)
Y<-factor(Y)
U<- as.data.frame(cbind(X,Y))
colnames(U)<- c("Rendimiento Melon","Variedad")
boxplot(U[,1]~U[,2],,xlab=paste(colnames(U)[2]),ylab=paste(colnames(U)[1]))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(U[,1]~U[,2],col=c('aquamarine1','yellow','orange','red'),pch=19,add=T)
legend(x = "topright",legend=c("1","2","3","4"),
       col=c('aquamarine1','yellow','orange', 'red'),pt.cex=1,pch=15,title='Variedad',
       box.lwd=1,text.font =20,cex=0.8)
U$Variedad
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
pf(23.36,3,21)
