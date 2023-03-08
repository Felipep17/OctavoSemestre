library(mixtools)
library(alr4)
library(depth)
X<-UN11
attach(X)
Y<- cbind(log(ppgdp),pctUrban)
plot(log(ppgdp),fertility)
plot(pctUrban,fertility)
plot(Y,xlab='pctUrban',ylab='Log(ppgdp)')
clcov<- cov(Y)
clcenter<- as.vector(colMeans(Y))
model<- lm(fertility~log(ppgdp)+pctUrban,data=X)
##########################
library(ddalpha)
depth.y<-depth.halfspace(Y,Y,num.directions=10000,seed=1)
sort.depth.Y<-sort(depth.y,decreasin=TRUE,index.return=TRUE)
depth.Y.sort<-sort.depth.Y$x
depth.Y.sort.index<-sort.depth.Y$ix
median=sort.depth.Y$ix[1]
#Gráfica de profundidad tukey general
par(mar=c(5,5,5,5))
par(mfrow=c(1,1))
plot(Y,ylim=c(0,120),xlim=c(4,12),xlab="logppgdp")
points(log(ppgdp)[median],pctUrban[median],pch=19,lwd=2,cex=1,col="aquamarine")
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.1,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,lty=3,lwd=3)
hii<-hatvalues(model)

p<- length(coefficients(model))
n<- nrow(X)
hii.c<- 2*(p/n)

indices<- (1:nrow(UN11))[hii>hii.c]
points(log(ppgdp)[indices],pctUrban[indices],col="red",pch=19)
text(log(ppgdp)[indices],pctUrban[indices],labels=rownames(UN11)[indices],pos=3)
plot(hii,type="h")
abline(h=hii.c,lty=2)
###############################
#MCD estimators
library(robustbase)
res=covMcd(Y)
mcdcenter=res$center
mcdcov=res$cov

#Ellipse 97.5% with robust MCD estimators
mixtools::ellipse(mu = mean_mcd, sigma = cov_mcd, alpha = 0.025,col = "red", lty = 2,lwd=2)
library(zoom)
zm()
#########
require(rrcov) 
data(hbk)
mcd <- rrcov::CovMcd(Y) # use only first three columns 
# get mcd estimate of location 
mean_mcd <- mcd$raw.center
# get mcd estimate scatter 
cov_mcd <- mcd$raw.cov
# get inverse of scatter 
cov_mcd_inv <- solve(cov_mcd) 
# compute distances 
# compute the robust distance 
robust_dist <- apply(Y, 1, function(x){
  x <- (x - mean_mcd) 
  dist <- sqrt((t(x) %*% cov_mcd_inv %*% x)) 
  return(dist) 
}) 
# set cutoff using chi square distribution 
threshold <- sqrt(qchisq(p = 0.975, df = ncol(hbk[,1:3]))) 
# df = no of columns # find outliers 
outliers <- which(robust_dist >= threshold) 
# gives the row numbers of outli
points(log(ppgdp)[outliers],pctUrban[outliers],pch=19,col="purple")
text(log(ppgdp)[outliers],pctUrban[outliers],labels=rownames(UN11)[outliers],pos=3)
zm()
med(Y,method="Spatial")
#########################################################################
p<- length(coefficients(model))
n<- nrow(X)
plot(fitted.values(model),studres(model))
summary(model)
abline(h=c(0,2,-2),lty=2,lwd=2)
hii<- hatvalues(model)
plot(hatvalues(model),type='h')
hii.c<- 2*(p/n)
###########################################################################
plot(hii,studres(model),pch=19)
abline(h=c(0,2,-2),lty=2,lwd=2)
abline(v=hii.c,lty=2,lwd=2)
studi<- studres(model)
##########################
plot(hii,studres(model),pch=19,panel.first=grid())
X[indices.1,]
X[indices.2,]
X[indices.3,]
indices.1<-(1:nrow(X))[hii<hii.c-0.01 & abs(studi)>2.5]
indices.2<-(1:nrow(X))[hii>hii.c-0.001 & abs(studi)> 2]
indices.3<- (1:nrow(X))[hii>hii.c+0.02& abs(studi)< 2]
plot(hii,studi,pch=19,xlab="Valores de la diagonal de la matriz hat", ylab=" Residuos Student")
abline(h=c(1,0,-1)*2,lty=2,v=hii.c)
points(hii[indices.3],studi[indices.3],col="yellow",pch=19)
text(hii[indices.3],studi[indices.3],labels=rownames(X)[indices.3],pos=2)
points(hii[indices.2],studi[indices.2],col="red",pch=19)
text(hii[indices.2],studi[indices.2],labels=rownames(X)[indices.2],pos=4)
points(hii[indices.1],studi[indices.1],col="aquamarine",pch=19)
text(hii[indices.1],studi[indices.1],labels=rownames(X)[indices.1],pos=c(1,3,4))
legend(x = "topright",legend=c("Influyente","Balanceo","Atípico"),
       col = c("red","yellow","aquamarine"),pch=c(19,19,19),pt.cex=2,
       box.lwd=0.6,title="Identificación de puntos",text.font =15,cex=0.8)
###############################################################################
library(car)
ck<- cooks.distance(model)
plot(ck,ylab="Distancia de Coock",pch=19,ylim=c(min(ck),max(ck)+0.1),panel.first=grid())
ck.c<- 4/n
abline(h=ck.c,lty=2)
indices<- (1:nrow(X))[ck>ck.c+0.02]
ck<- ck[ck>ck.c+0.02]
points(indices,ck,col="red",pch=19)
text(indices,ck,labels=rownames(X)[indices],pos=c(1,1,3),cex=0.6)
influencePlot(model)
######################################
