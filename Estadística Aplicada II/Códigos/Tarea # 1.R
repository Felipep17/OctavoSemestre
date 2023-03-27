##############################
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Estadística Aplicada II/Base de datos")
library(mixtools)
library(alr4)
library(depth)
library(readr)
library(readxl)
library(ddalpha)
library(robustbase)
require(rrcov) 
library(zoom)
library(ggfortify)
library(psych)
library(car)
###### Funciones creadas por el estudiante
# Estadística Descriptivas
resumen<- function(x){
  X<- matrix(0,9,1)
  resumen<- round(c(mean(x),median(x),min(x),max(x),var(x),sd(x),quantile(x,0.25),quantile(x,0.75),100*(sd(x)/mean(x))),9)
  for( i  in 1:9){
    X[i,]<- resumen[i]
  }
  rownames(X)<-c('Media','Mediana','Min','Max','Var','Sd','1st Qu.','3rd Qu','Coef.Var')
  colnames(X)<- (paste(colnames(x)))
  return(X)
}
################################ Importación de los datos
data <- read_excel("data.xlsx")
X<-data
################ Selección de las variables dadas por el docente
X<- cbind(X[,1:30],X[,colnames(X)=='density'])
#Dimensión del dataframe
p<-dim(X)[2]
n<- dim(X)[1]
##############
head(X)
# Estadística descriptivas
summary<- matrix(0,p,9)
for(i in 1:p){
  summary[i,]<- t(resumen(X[,i]))
}
rownames(summary)<-colnames(X)
colnames(summary)<-c('Media','Mediana','Min','Max','Var','Sd','1st Qu.','3rd Qu','Coef.Var %')
summary
#Pasar de R a Latéx
xtable(summary)
# Matriz de correlación
psych::pairs.panels(X[,1:10], 
             method = "pearson", # correlation method
             hist.col = "aquamarine1",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
psych::pairs.panels(X[11:20], 
                    method = "pearson", # correlation method
                    hist.col = "aquamarine1",
                    density = TRUE,  # show density plots
                    ellipses = TRUE # show correlation ellipses
)
psych::pairs.panels(X[21:31], 
                    method = "pearson", # correlation method
                    hist.col = "aquamarine1",
                    density = TRUE,  # show density plots
                    ellipses = TRUE # show correlation ellipses
)
#Visualización de los datos
set.seed(1)
#Variable aleatoria para crear la gráfica
sample(1:30,1)
#Creación del panel de fondo
plot(seq(min(X[,25]),max(X[,25]),length.out=30),seq(min(X[,31]),max(X[,31]),length.out=30),type='n',xlab='',ylab='')
grid(10,10,col=c('aquamarine3','blue4'))
par(new=T)
plot(X[,31]~X[,25],ylab='Densidad',xlab=' NIR 25',pch=19)
model<- lm(density~`NIR 25`,data=X)
abline(model,lwd=2)
summary(model)
qqPlot(model)
### Tabla ANOVA
anova(model)
########### Análisis exploratorio de datos atípicos e influyentes
attach(X)
Y<- cbind(`NIR 25`,density)
clcov<- cov(Y)
clcenter<- as.vector(colMeans(Y))
model<- lm(density~`NIR 25`,data=X)
##########################

depth.y<-depth.halfspace(Y,Y,num.directions=10000,seed=1)
sort.depth.Y<-sort(depth.y,decreasin=TRUE,index.return=TRUE)
depth.Y.sort<-sort.depth.Y$x
depth.Y.sort.index<-sort.depth.Y$ix
median=sort.depth.Y$ix[1]
#Gráfica de profundidad tukey general
plot(seq(min(X[,25]),max(X[,25]),length.out=30),seq(min(X[,31]),max(X[,31]),length.out=30),type='n',xlab='',ylab='')
grid(10,10,col=c('aquamarine3','blue4'))
par(new=T)
plot(X[,31]~X[,25],ylab='Densidad',xlab=' NIR 25',pch=19)
points(`NIR 25`[median],density[median],pch=19,lwd=2,cex=1,col="aquamarine")
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.1,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,lty=3,lwd=3)
hii<-hatvalues(model)
#Para usar la distancia de cook
p<- length(coefficients(model))
n<- nrow(X)
hii.c<- 2*(p/n)

indices<- (1:nrow(X))[hii>hii.c]
points(`NIR 25`[indices],density[indices],col="red",pch=19)
text(`NIR 25`[indices],density[indices],labels=rownames(X)[indices],pos=3)
plot(hii,type="h")
abline(h=hii.c,lty=2)
###############################
#MCD estimators

res=covMcd(Y)

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
points(`NIR 25`[outliers],density[outliers],pch=19,col="purple")
text(`NIR 25`[outliers],density[outliers],labels=rownames(X)[outliers],pos=3)

zm()
med(Y,method="Spatial")
