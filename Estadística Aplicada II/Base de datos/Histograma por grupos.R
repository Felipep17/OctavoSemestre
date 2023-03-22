x<- rnorm(1000,10,1)
g<- as.factor(c(rep('1',50),rep('2',50)))
Df<- as.data.frame(cbind(x,g))
head(Df)
mi.azul <- rgb(0, 0, 1, 0.3) # rgb(0, 0, 1, alpha = 0.3)
mi.rojo <- rgb(1, 0, 0, 0.5)
par(mfrow=c(1,1))

hist(Df$x[Df$g=='1'],xlim=c(range(Df$x)),col=mi.azul)
# Grid vertical
axis(1, tck = 1, lty = 2, col = "gray")

# Grid horizontal
axis(2, tck = 1, lty = 2, col = "gray")
par(new=T)
hist(Df$x[Df$g=='2'],xlim=c(range(Df$x)),axes=F,col=mi.rojo,xlab='',ylab='',main='')
legend(x = "topright",legend=c("G1","G2"),pch=15
       ,pt.cex=2,col=c(mi.azul,mi.rojo),
       box.lwd=0.6,text.font =15,cex=1)

