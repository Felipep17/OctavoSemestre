##############################
library(easypackages)
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Estadística Aplicada II/Base de datos")
lib_req<-c('psych','car','lmtest','MASS','xtable','latex2exp','orcutt','nlme',
           'mixtools',"alr4","depth","readr","ddalpha","robustbase","rrcov","zoom",'ggfortify','readxl','zm')# Listado de librerias requeridas por el script
easypackages::packages(lib_req)    
options(scipen=999)
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
# Análisis exploratorio visual
exploratorio<- function(X){
  psych::pairs.panels(X, 
                      method = "pearson", # correlation method
                      hist.col = "aquamarine1",
                      density = TRUE,  # show density plots
                      ellipses = TRUE # show correlation ellipses
  )
}
########Validación de supuestos
validaciongrafica<- function(model,cor=F){
  crPlots(model,main='Residuos Parciales')
  par(mfrow=c(1,2))
  plot(fitted.values(model),studres(model),panel.first=grid(),pch=19,ylab='Residuos Estudentizados',xlab='Valores ajustados',main='A',col='aquamarine4')
  lines(lowess(studres(model)~fitted.values(model)), col = "red1")
  abline(h=c(-2,0,2),lty=2)
  qqPlot(model,pch=19,ylab='Residuos Estudentizados',xlab='Cuantiles Teóricos',col=carPalette()[1],col.lines=carPalette()[3],main='B')
  print('Shapiro Test')
  print(shapiro.test(studres(model)))
  print('Breusch Pagan Test')
  print(bptest(model))
  if(cor==T){
  par(mfrow=c(1,2))
  plot(studres(model),type="b",xlab="Tiempo",ylab="Residuos Estudentizados",main="A",pch=19,panel.first=grid())
  plot(studres(model)[-length(fitted.values(model))],studres(model)[-1],pch=19,panel.first = grid(),col="turquoise3",xlab=TeX("$Residuos_{t-1}$"),ylab=TeX("$Residuos_{t}$"),main="B")
  abline(lm(studres(model)[-1]~studres(model)[-length(fitted.values(model))]))
  print('Durbin Watson Test')
  print(durbinWatsonTest(model,method='resample',reps=10000))
  }
}
#Cálculo del lambda óptimo para el boxcox
lambda<- function(model,a,b){
  par(mfrow=c(1,1))
  box.cox<-boxcox(model,lambda=seq(a,b,length.out = 1000),
         ylab='log-verosimilitud')
print(box.cox)
  bc<-round(box.cox$x[box.cox$y ==max(box.cox$y)],2)
  print(bc)
}
####### Validación supuestos MCP
validacionmcp<- function(model){
  par(mfrow=c(1,2))
  res.ponderados<- residuals(model)*sqrt(weights(model))
  print(plot(fitted.values(model),res.ponderados,
             xlab='valores ajustados',main='A',ylab='residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4"))
  lines(lowess(res.ponderados~fitted.values(model)),col=2,lty=2,lwd=4)
  abline(h=0,lty=2,lwd=2)
  print(qqPlot(res.ponderados,pch=19,xlab='Cuantiles Teóricos',ylab='Residuos Ponderados',col=carPalette()[1],col.lines=carPalette()[3],main='B'))
  print(shapiro.test(res.ponderados))
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
View(summary)
#Pasar de R a Latéx
xtable(summary)
# Matriz de correlación

exploratorio(X[,1:10])
exploratorio(X[,11:20])
exploratorio(X[,21:31])
#Visualización de los datos
set.seed(11)
#Variable aleatoria para crear la gráfica
sample(1:30,1)
#Creación del panel de fondo
par(mfrow=c(1,1))
plot(seq(min(X[,26]),max(X[,26]),length.out=30),seq(min(X[,31]),max(X[,31]),length.out=30),type='n',xlab='',ylab='')
grid(10,10,col=c('aquamarine3','blue4'))
par(new=T)
plot(X[,31]~X[,26],ylab='Densidad',xlab=' NIR 26',pch=19)
model<- lm(density+0.001~NIR26,data=X)
abline(model,lwd=2)
#Validación de supuestos gráfica
validaciongrafica(model,cor=F)
##############
Z<- as.data.frame(cbind(X[,31],X[,26]))
colnames(Z)<-c('Density','NIR26')
set.seed(100)
ind<-sample(1:nrow(Z),nrow(Z))
Z<- as.data.frame(Z[ind,])
modelprueba<- lm(Density~NIR26,data=Z)
validaciongrafica(modelprueba,cor=T)
###########
par(mfrow=c(1,2))
acf(MASS::studres(model))
acf(MASS::studres(model),type='partial')
par(mfrow=c(1,1))
plot(studres(modelprueba)[-length(studres(model))],studres(modelprueba)[-1])
######## 
lambda(model,-3,3)
model.box<- lm(log(density+0.0001)~NIR26,data=X)
validaciongrafica(model.box)
########### Análisis exploratorio de datos atípicos e influyentes
attach(X)
Y<- cbind(NIR26,density)
clcov<- cov(Y)
clcenter<- as.vector(colMeans(Y))
model<- lm(density~NIR26,data=X)
##########################
depth.y<-depth.halfspace(Y,Y,num.directions=10000,seed=1)
sort.depth.Y<-sort(depth.y,decreasin=TRUE,index.return=TRUE)
depth.Y.sort<-sort.depth.Y$x
depth.Y.sort.index<-sort.depth.Y$ix
median=sort.depth.Y$ix[1]
#Gráfica de profundidad tukey general
par(mfrow=c(1,1))
plot(seq(min(X[,25]),max(X[,25]),length.out=30),seq(min(X[,31]),max(X[,31]),length.out=30),type='n',xlab='',ylab='')
grid(10,10,col=c('aquamarine3','blue4'))
par(new=T)
plot(X[,31]~X[,25],ylab='Densidad',xlab=' NIR 25',pch=19)
points(NIR26[median],density[median],pch=19,lwd=2,cex=1,col="aquamarine")
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.1,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.01,lty=3,lwd=3)
###############################
#MCD estimators
zm()
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
points(NIR26[outliers],density[outliers],pch=19,col="purple")
text(NIR26[outliers],density[outliers],labels=rownames(X)[outliers],pos=3)

zm()
med(Y,method="Spatial")
################ REGRESIÓN CON Correlación
######## ALTERNATIVA no nesaria pero que no esta de más tener no para este caso
# Sino para un futuro
summary(gls(density~NIR26,data=X))
ñ<- gls(density~NIR25)
shapiro.test(residuals(ñ))
mcor<-lm(density~NIR25,data=X)
mcor1<-cochrane.orcutt(mcor)
summary(mcor1)
qqPlot(residuals(mcor1))
coefficients(mcor1)
