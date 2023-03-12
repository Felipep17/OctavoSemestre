X<- as.numeric(c(10,9.6,9.6,9.2,9.2,7.6,8,7.5,8.4,8.2,10.8,10,9.6,9.8,10))
Y<- as.character(c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"))
Y<-factor(Y)
U<- as.data.frame(cbind(X,Y))
head(U)
colnames(U)<- c("Crecimiento en cm","Tipo de Hormona")
boxplot(U[,1]~U[,2],col=c('aquamarine1','red1','orange'),xlab=paste(colnames(U)[2]),ylab=paste(colnames(U)[1]),pch=19,main='Diagrama de cajas para el crecimiento en Cm de los ratones según el tipo de hormona')
legend(x = "bottomright",legend=c("Hormona A","Hormona B","Hormona C"),
       col=c('aquamarine1','red1','orange'),pt.cex=1,pch=15,title='Tipo de Hormona',
       box.lwd=0.6,text.font =15,cex=0.6)
####### Creación de tabla
l<-matrix(c(0.2,0.15,1000,0.2,0.15,2000,0.2,0.15,3000,0.2,0.2,1000,0.2,0.2,2000,0.2,0.2,3000,0.25,0.15,1000,0.25,0.15,2000,0.25,0.15,3000,0.25,0.2,1000,0.25,0.2,2000,0.25,0.2,3000,0.3,0.15,1000,0.3,0.15,2000,0.3,0.15,3000,0.3,0.2,1000,0.3,0.2,2000,0.3,0.2,3000),18,3,T)
colnames(l)<- c("Rap.Alimentación (plg/min)","Prof.Corte (plg)","Rota.Herramienta (rpm)")
library(xtable)
xtable(l)
