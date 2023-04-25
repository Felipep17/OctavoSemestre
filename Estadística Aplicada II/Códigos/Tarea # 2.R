##############################
options(scipen=999)
#
library(easypackages)
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Estadística Aplicada II/Base de datos")
lib_req<-c("scatterplot3d","plot3D","plotly","rgl","plot3Drgl",'effects','psych','car','lmtest','MASS','xtable','latex2exp','orcutt','nlme',
           'mixtools',"alr4","depth","readr","ddalpha","robustbase","rrcov","zoom",'ggfortify','readxl')# Listado de librerias requeridas por el script
easypackages::packages(lib_req)    
###### Funciones creadas por el estudiante
# EstadC-stica Descriptivas
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
# AnC!lisis exploratorio visual
exploratorio<- function(X){
  psych::pairs.panels(X, 
                      method = "pearson", # correlation method
                      hist.col = "aquamarine1",
                      density = TRUE,  # show density plots
                      ellipses = TRUE # show correlation ellipses
  )
}
########ValidaciC3n de supuestos
validaciongrafica<- function(model,cor=F){
  crPlots(model,main='Residuos Parciales')
  par(mfrow=c(1,2))
  plot(fitted.values(model),studres(model),panel.first=grid(),pch=19,ylab='Residuos Estudentizados',xlab='Valores ajustados',main='A',col='aquamarine4')
  lines(lowess(studres(model)~fitted.values(model)), col = "red1")
  abline(h=c(-2,0,2),lty=2)
  qqPlot(model,pch=19,ylab='Residuos Estudentizados',xlab='Cuantiles TeC3ricos',col=carPalette()[1],col.lines=carPalette()[3],main='B')
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
#CC!lculo del lambda C3ptimo para el boxcox
lambda<- function(model,a,b){
  par(mfrow=c(1,1))
  box.cox<-boxcox(model,lambda=seq(a,b,length.out = 10000),
                  ylab='log-verosimilitud')
  print(box.cox)
  bc<-round(box.cox$x[box.cox$y ==max(box.cox$y)],2)
  print(bc)
}
####### ValidaciC3n supuestos MCP
validacionmcp<- function(model){
  crPlots(model)
  par(mfrow=c(1,2))
  res.ponderados<- residuals(model)*sqrt(weights(model))
  print(plot(fitted.values(model),res.ponderados,
             xlab='valores ajustados',main='A',ylab='residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4"))
  lines(lowess(res.ponderados~fitted.values(model)),col=2,lty=2,lwd=4)
  abline(h=0,lty=2,lwd=2)
  print(qqPlot(res.ponderados,pch=19,xlab='Cuantiles TeC3ricos',ylab='Residuos Ponderados',col=carPalette()[1],col.lines=carPalette()[3],main='B'))
  print(shapiro.test(res.ponderados))
}
################################ ImportaciC3n de los datos
data <- read_excel("data.xlsx")
X<-data
################ SelecciC3n de las variables dadas por el docente
X<- cbind(X[,1:30],X[,colnames(X)=='density'])
Y<-cor(X)
Y<-Y[,-31]
cor<-c(max(Y[31,]),min(Y[31,]))
which(Y[31,]==cor[1])
which(Y[31,]==cor[2])
#DimensiC3n del dataframe
p<-dim(X)[2]
n<- dim(X)[1]
##############
head(X)
# Matriz de correlaciC3n
Sd <- apply(X[,c(4,29,31)],2,sd)
RESU <- rbind(apply(X[,c(4,29,31)],2,summary),Sd)
RESU
#
model<- lm(density+0.0001~NIR4+NIR29,data=X)
vif(model)
summary(model)
plot(X[,c(4,29,31)],pch=19,panel.first=grid())
z<-X[,31];y<-X[,4];x<-X[,29];
scatter3D(x, y, z, phi = 0, bty = "b2",col = c('aquamarine','aquamarine2','aquamarine3','dodgerblue','dodgerblue2','dodgerblue3','dodgerblue4'),
          pch = 20, cex = 2, ticktype = "detailed")
plotrgl()
#La variable Z es la variable a predecir
#Creamos un objeto para realizar las predicciones con elmodelo
objr<-lm(z ~ x+y)
summary(objr)
#preparamos el modelado 3d
grid.lines = 42
x.pred <- seq(min(x), max(x), length.out = grid.lines)


y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(objr, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# Marcamos las líneas de iteracción para que busquen la recta de regresión
fitpoints <- predict(objr)
#ploteamos la gráfica en 3d con recta de regresión
scatter3D(x, y, z, pch = 19, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "",xlab='NIR29 ',zlab="Density",ylab='NIR4', col = c('aquamarine','aquamarine2','aquamarine3','dodgerblue','dodgerblue2','dodgerblue3','dodgerblue4'))
#Gráfico dinámico
plotrgl()
influence.measures(model)
#ValidaciC3n de supuestos gráfica
validaciongrafica(model,cor=F)
#####
summary(model)
#Intervalos de confianza
confint(model)
##############
anova(model)
###########
########  BOX-COX
lambda(model,-5,10)
model.box<- lm((density^0.65)~NIR29+NIR4,data=X)
validaciongrafica(model.box)
###### Minimos cuadrados ponderados
res.mcp<- residuals(model)
varianza<- lm(abs(res.mcp)~NIR29+NIR4,data=X)
w = 1/(fitted.values(varianza)^2)
model.ponderados<- lm(density~NIR29+NIR24,data=X,weights = w)
validacionmcp(model.ponderados)
summary(model.ponderados)
########### AnC!lisis exploratorio de datos atC-picos e influyentes
attach(X)
Y<- cbind(NIR18,density)
clcov<- cov(Y)
clcenter<- as.vector(colMeans(Y))
model<- lm(density~NIR18,data=X)
##########################
depth.y<-depth.halfspace(Y,Y,num.directions=10000,seed=1)
sort.depth.Y<-sort(depth.y,decreasin=TRUE,index.return=TRUE)
depth.Y.sort<-sort.depth.Y$x
depth.Y.sort.index<-sort.depth.Y$ix
median=sort.depth.Y$ix[1]
#GrC!fica de profundidad tukey general
par(mfrow=c(1,1))
plot(seq(min(X[,18]),max(X[,18]),length.out=30),seq(min(X[,31]),max(X[,31]),length.out=30),type='n',xlab='',ylab='')
grid(10,10,col=c('aquamarine3','blue4'))
par(new=T)
plot(X[,31]~X[,18],ylab='Densidad',xlab=' NIR 18',pch=19)
points(NIR18[median],density[median],pch=19,lwd=2,cex=1,col="aquamarine")
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.1,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.01,lty=3,lwd=3)
########Puntos fuera de la elipse
# get inverse of scatter 
cov_ <- solve(clcov) 
# compute distances 
# compute the robust distance 
robust_dist <- apply(Y, 1, function(x){
  x <- (x - clcenter) 
  dist <- sqrt((t(x) %*% cov_ %*% x)) 
  return(dist) 
}) 
# set cutoff using chi square distribution 
threshold. <- sqrt(qchisq(p = 0.975, df = ncol(Y))) 
# df = no of columns # find outliers 
outliers. <- which(robust_dist >= threshold) 
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
points(NIR18[outliers],density[outliers],pch=19,col="purple")
text(NIR18[outliers],density[outliers],labels=rownames(X)[outliers],pos=3)

zm()
################################## IDENTIFICACICN PUNTOS ATCPICOS
influence.measures(model.ponderados)
######
# E influyentes
influencePlot(model.ponderados)
##########
#################### IdentificaciC3n de puntos atC-picos,balanceo e influyentes
#Puntos de Balanceo, Influyentes y AtC-picos

res.ponderados<- residuals(model.ponderados)*weights(model.ponderados)
par(mfrow=c(1,1))
p<- length(coefficients(model.ponderados))
n<- nrow(X)
hii.c<- 2*p/n
influencePlot(model.ponderados,panel.first=grid())
hii<- hatvalues(model.ponderados)
hii.ind<- hii[hii>hii.c]
plot(hii,ylab="Valores diagonal de la matriz Hat",pch=19,xlab="IndC-ces",ylim=c(0,0.3),panel.first=grid())
points((1:nrow(X))[hii>hii.c],hii.ind,col="red",pch=19)
text((1:nrow(X))[hii>hii.c],hii.ind,labels=rownames(X)[(1:nrow(X))[hii>hii.c]],pos=c(1,2,3,3,3,1,3,3,1),cex=0.8)
abline(h=2*p/n,lty=2)
n<- length(residuals(model.ponderados))
p<- length(coefficients(model.ponderados))
hii.c<-2*(p/n)
abline(h=hii.c,lty=2,lwd=2)
indices.1<-(1:nrow(X))[hii<hii.c & abs(res.ponderados)>2]
indices.2<-(1:nrow(X))[hii>hii.c & abs(res.ponderados)> 2]
indices.3<- (1:nrow(X))[hii>hii.c& abs(res.ponderados)< 2]
plot(hii,res.ponderados,pch=19,xlab="Valores de la diagonal de la matriz hat", ylab=" Residuos Ponderados",ylim=c(-2,2),xlim=c(0,0.25),panel.first=grid())
abline(h=c(1,0,-1)*2,lty=2,v=hii.c)
points(hii[indices.3],res.ponderados[indices.3],col="yellow",pch=19)
text(hii[indices.3],res.ponderados[indices.3],labels=rownames(X)[indices.3],pos=3)
points(hii[indices.2],res.ponderados[indices.2],col="red",pch=19)
text(hii[indices.2],res.ponderados[indices.2],labels=rownames(X)[indices.2],pos=4)
points(hii[indices.1],res.ponderados[indices.1],col="aquamarine",pch=19)
text(hii[indices.1],res.ponderados[indices.1],labels=rownames(X)[indices.1],pos=c(1,3,4))
legend(x = "topright",legend=c("Influyente","Balanceo","AtC-pico"),
       col = c("red","yellow","aquamarine"),pch=c(19,19,19),pt.cex=2,
       box.lwd=0.6,title="IdentificaciC3n de puntos",text.font =15,cex=0.6)
indices.6<-which(X$salary>3200 & X$salary<3500)
summary(model.ponderados)
############## Distancia de Cook
ck<- cooks.distance(model.ponderados)
plot(ck,ylab="Distancia de Coock",pch=19,ylim=c(min(ck),max(ck)+0.1),panel.first=grid())
ck.c<- 4/n
abline(h=ck.c,lty=2)
indices<- (1:nrow(X))[ck>ck.c]
ck<- ck[ck>ck.c]
points(indices,ck,col="red",pch=19)
text(indices,ck,labels=rownames(X)[indices],pos=3,cex=0.6)
influencePlot(model.ponderados)
########### DfBetas
#Beta 1
par(mfrow=c(1,1))
DFBETAS = dfbetas(model.ponderados)
DFBETAS
plot(DFBETAS[,2],ylab=quote('DFBETA'~(beta[1])),xlab="IndC-ce",pch=19,ylim=c(-0.4,0.5),xlim=c(0,150),panel.first=grid())
ind = (1:nrow(X))[abs(DFBETAS[,2]) > 2/sqrt(nrow(X))]
dfb = DFBETAS[abs(DFBETAS[,2]) > 2/sqrt(nrow(X)) ,2]
abline(h=c(1,-1)*2/sqrt(nrow(X)))
text(ind,dfb,rownames(X)[abs(DFBETAS[,2]) > 2/sqrt(nrow(X))],pos=c(1,3,1,4,3,2,1,4,3,4),
     cex=0.8)
points(ind,dfb,col="red",pch=19)
head(DFBETAS)
################
################ Dffits
par(mfrow=c(1,1))
DFFITS = dffits(model.ponderados)
plot(DFFITS,xlab="IndC-ces",pch=19,ylim=c(-1,1),panel.first=grid())
abline(h=c(-1,1)*2*sqrt(p/n))
ind = (1:nrow(X))[abs(DFFITS) > 2*sqrt(p/n)]
dfb = DFFITS[abs(DFFITS) > 2*sqrt(p/n)]
text(ind,dfb,rownames(X)[abs(DFFITS) > 2*sqrt(p/n)],pos=2)
points(ind,dfb,col="purple4",pch=19)
################ CovRatio
COVR = covratio(model.ponderados)
plot(COVR,pch=19,ylab="Covratio",xlab="IndC-ce",panel.first=grid())
abline(h=1+c(-1,1)*3*(p/n))
covr = COVR[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
ind = (1:nrow(X))[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
text(ind,covr,rownames(X)[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n)],pos=4)
points(ind,covr,col="purple4",pch=19)
################ REGRESICN CON CorrelaciC3n
######## ALTERNATIVA no nesaria pero que no esta de ms tener no para este caso
#Modelo estandarizado
W<- as.data.frame(scale(Y)*1/(sqrt(nrow(Y)-1)))
model.scale<- lm(density~NIR18,data=W)
validaciongrafica(model.scale)
validaciongrafica(model)
summary(model)
summary(model.scale)
# Sino para un futuro#####################
#######################
###########
summary(gls(density~NIR26,data=X))
C1<- gls(density~NIR25)
shapiro.test(residuals(C1))
mcor<-lm(density~NIR25,data=X)
mcor1<-cochrane.orcutt(mcor)
summary(mcor1)
qqPlot(residuals(mcor1))
coefficients(mcor1)
influence.measures(model.ponderados)
####################
model.<- lm(density~NIR29+I(NIR29^2),data=X)
summary(model.)
plot(X[,29],X[,31],pch=19,col="#FF8C00",panel.first=grid(),xlab="NIR29",
     ylab="densidad del hilo PET",main='Densidad Vs NIR29')
#Gráficas de las lineas
splines(X[,29],fitted.values(model.),lty=2,lwd=3)
lines(spline(X[,29],fitted.values(model.)),lwd=2)
abline(model.ponderados,lwd=2,lty=2)
autoplot(model.)
validaciongrafica(model.)
###### Minimos cuadrados ponderados
res.mcp<- residuals(model.)
varianza<- lm(abs(res.mcp)~NIR29+I(NIR29^2),data=X)
w = 1/(fitted.values(varianza)^2)
model.ponderados<- lm(density~NIR29+I(NIR29^2),data=X,weights = w)
validacionmcp(model.ponderados)
summary(model.ponderados)
