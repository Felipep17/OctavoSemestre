library(pls)
X<-as.data.frame(yarn$NIR)
y<- as.data.frame(yarn$density)
z<- as.data.frame(yarn$train)
library(openxlsx)
write.xlsx(X,'NIR.xlsx')
write.xlsx(y,'density.xlsx')
write.xlsx(z,'train.xlsx')
yarn$density
View(yarn)
library(readr)
X <- data
ncol(X)
plot(X[,1:10])
plot(density~NIR.50,data=X,pch=19,panel.first=grid())
x<- X[,50]
summary(x)
resumen<- function(x){
  X<- matrix(0,9,1)
  resumen<- round(c(mean(x),median(x),min(x),max(x),var(x),sd(x),quantile(x,0.25),quantile(x,0.75),sd(x)/mean(x)),4)
  for( i  in 1:7){
    X[i,]<- resumen[i]
  }
  rownames(X)<-c('Media','Mediana','Min','Max','Var','Sd','1st Qu.','3rd Qu','Coef.Var')
  colnames(X)<- ('Estadística Descriptivas')
  return(X)
}
resumen(x)
library(xtable)
xtable(resumen(x))
##############################
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Estadística Aplicada II/Base de datos")
library(mixtools)
library(alr4)
library(depth)
library(readr)
library(readxl)
data <- read_excel("data.xlsx")
X<-data
dim(X)
attach(X)
Y<- cbind(`NIR 50`,density)
clcov<- cov(Y)
clcenter<- as.vector(colMeans(Y))
model<- lm(density~`NIR 50`,data=X)
##########################
library(ddalpha)
depth.y<-depth.halfspace(Y,Y,num.directions=10000,seed=1)
sort.depth.Y<-sort(depth.y,decreasin=TRUE,index.return=TRUE)
depth.Y.sort<-sort.depth.Y$x
depth.Y.sort.index<-sort.depth.Y$ix
median=sort.depth.Y$ix[1]
X$`NIR 50`[25]
#Gráfica de profundidad tukey general
par(mar=c(5,5,5,5))
par(mfrow=c(1,1))
plot(X$density~X$`NIR 50`,xlab="NIR.50",ylab='Density',pch=19,panel.first=grid())
points(`NIR 50`[median],density[median],pch=19,lwd=2,cex=1,col="aquamarine")
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.1,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,lty=3,lwd=3)
hii<-hatvalues(model)

p<- length(coefficients(model))
n<- nrow(X)
hii.c<- 2*(p/n)

indices<- (1:nrow(X))[hii>hii.c]
points(`NIR 50`[indices],density[indices],col="red",pch=19)
text(`NIR 50`[indices],density[indices],labels=rownames(X)[indices],pos=3)
plot(hii,type="h")
abline(h=hii.c,lty=2)
###############################
#MCD estimators
library(robustbase)
res=covMcd(Y)
require(rrcov) 
mcd <- rrcov::CovMcd(Y) # use only first three columns 

mcdcenter=res$center
mcdcov=res$cov
# get mcd estimate of location 
mean_mcd <- mcd$raw.center
# get mcd estimate scatter 
cov_mcd <- mcd$raw.cov

#Ellipse 97.5% with robust MCD estimators
mixtools::ellipse(mu = mean_mcd, sigma = cov_mcd, alpha = 0.025,col = "red", lty = 2,lwd=2)
#########

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
threshold <- sqrt(qchisq(p = 0.975, df = ncol(Y))) 
# df = no of columns # find outliers 
outliers <- which(robust_dist >= threshold) 
# gives the row numbers of outli
points(`NIR 50`[outliers],density[outliers],pch=19,col="purple")
text(`NIR 50`[outliers],density[outliers],labels=rownames(X)[outliers],pos=3)
library(zoom)
zm()
med(Y,method="Spatial")
View(X)
X<- cbind(X[,1:30],X[,colnames(X)=='density'])
cor(X)
#Guardar
set.seed(1)
sample(1:30,1)
plot(seq(min(X[,25]),max(X[,25]),length.out=30),seq(min(X[,31]),max(X[,31]),length.out=30),type='n',xlab='',ylab='')
grid(10,10,col=c('yellow3','blue4'))
par(new=T)
plot(X[,31]~X[,25],ylab='Densidad',xlab=' Nir.25',pch=19)
model<- lm(density~`NIR 25`,data=X)
abline(model)
summary(model)
library(ggfortify)
autoplot(model)
