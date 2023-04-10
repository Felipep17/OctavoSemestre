##############################
options(scipen=999)
# Cargo las librerías
library(easypackages)
lib_req<-c('tidyr','pls','psych','lmridge','ISLR','car','lmtest','MASS','xtable','latex2exp','orcutt','nlme',
           'mixtools',"alr4","depth","readr","ddalpha","robustbase","rrcov","zoom",'ggfortify','readxl')# Listado de librerias requeridas por el script
easypackages::packages(lib_req) 
###Funciones necesarias
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
  par(mfrow=c(1,1))
}
#Cálculo del lambda óptimo para el boxcox
lambda<- function(model,a,b){
  par(mfrow=c(1,1))
  box.cox<-boxcox(model,lambda=seq(a,b,length.out = 1000),
                  ylab='log-verosimilitud')
  bc<-round(box.cox$x[box.cox$y ==max(box.cox$y)],2)
  print(bc)
}
####### Validación supuestos MCP
validacionmcp<- function(model){
  crPlots(model)
  par(mfrow=c(1,2))
  res.ponderados<- residuals(model)*sqrt(weights(model))
  print(plot(fitted.values(model),res.ponderados,
             xlab='valores ajustados',main='A',ylab='residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4"))
  lines(lowess(res.ponderados~fitted.values(model)),col=2,lty=2,lwd=4)
  abline(h=0,lty=2,lwd=2)
  print(qqPlot(res.ponderados,pch=19,xlab='Cuantiles Teóricos',ylab='Residuos Ponderados',col=carPalette()[1],col.lines=carPalette()[3],main='B'))
  print(shapiro.test(res.ponderados))
}
# Variabilidad explicada por componentes principales
varcomp<-function(X){
  lambdas<-eigen(cor(X))$values
  vectors<- eigen(cor(X))$vectors
  prop.var <- lambdas / sum(lambdas) # Proporcion de variabilidad
  prop.var.accum <- cumsum(lambdas) / sum(lambdas) # Proporcion de variabilidad acumulada
  par(mfrow=c(1,1))
  plot(1:length(lambdas),prop.var,type='l',panel.first=grid(),ylim=c(0,1),xlab='Componente principal',ylab='Porcentaje variabilidad explicada')
  text(1:length(lambdas),prop.var,labels=as.character(paste(round(prop.var.accum*100,2),'%')),cex=0.8,pos=c(4,4,1,3,3,3,3,3))
  points(1:length(lambdas),prop.var,pch=19,col=c('aquamarine4'))
}

######## Regresión por componentes principales
PCR<- function(X,y,ncomp){
  Z<- scale(X)*(1/sqrt(nrow(X)-1))
  y.e<- y
  y<- scale(y)*(1/sqrt(nrow(X)-1))
  if(ncomp<=(ncol(Z)-1)&& ncomp>0){
    S<- Z%*%eigen(cor(Z))$vectors
    pcr<-lm(y~S-1)
    R2<-summary(lm(y~S[,-((ncomp+1):ncol(Z))]))$r.squared
    valphas<- vcov(pcr)
    alphas<- coefficients(pcr)
    alphas[((ncomp+1):ncol(Z))]<-0
    beta.CP1 =eigen(cor(Z))$vectors%*%alphas
    valphas[((ncomp+1):ncol(Z)),]<-0
    valphas[,((ncomp+1):ncol(Z))]<-0
    vbb<- eigen(cor(Z))$vectors%*%valphas%*%t(eigen(cor(Z))$vectors)
    t.value<- beta.CP1/diag(vbb)
    p.value<- (1-pt(abs(t.value),nrow(Z)-ncol(Z)))*2
    resumen<- cbind(round(beta.CP1,4),round(sqrt(diag(vbb)),4),round(t.value,4),round(p.value,4))
    rownames(resumen)<- colnames(Z)
    colnames(resumen)<- c('Estimate','Std.Error','t value',' Pr(> |t|)')
    print(resumen)
    cat('Multiple R-Squared',round(R2,4))
    return( list(R2=R2,betas=beta.CP1,summary=resumen,var=vbb,t.value=t.value,p.value=p.value))
  }
  if(ncomp==ncol(Z)){
    S<- Z%*%eigen(cor(Z))$vectors
    pcr<-lm(y~S-1)
    R2<-summary(lm(y~S-1))$r.squared
    valphas<- vcov(pcr)
    alphas<- coefficients(pcr)
    beta.CP1 =eigen(cor(Z))$vectors%*%alphas
    vbb<- eigen(cor(Z))$vectors%*%valphas%*%t(eigen(cor(Z))$vectors)
    t.value<- beta.CP1/diag(vbb)
    p.value<- (1-pt(abs(t.value),nrow(Z)-ncol(Z)))*2
    resumen<- cbind(round(beta.CP1,4),round(sqrt(diag(vbb)),4),round(t.value,4),round(p.value,4))
    rownames(resumen)<- colnames(Z)
    colnames(resumen)<- c('Estimate','Std.Error','t value',' Pr(> |t|)')
    print(resumen)
    cat('Multiple R-Squared',round(R2,4))
    return( list(R2=R2,betas=beta.CP1,summary=resumen,var=vbb,t.value=t.value,p.value=p.value))
  }
  else{
    cat('Warning')
  }
}
#Importación de la base de datos
X<- na.omit(ISLR::Hitters)
names(X)
ind<- c(19,2,9,4,11,3,10,5,12)
plot(X[,ind],panel.first=grid(),col='aquamarine4')
exploratorio(X[,ind])
X<- X[,ind]
names(X)
model<- lm(Salary~.,data=X)
summary(model)
vif<-as.data.frame(t(as.numeric(car::vif(model.box))))
names(X)
colnames(vif)<- colnames(X[,-1])
xtable(vif)
# Validación de supuestos
validaciongrafica(model,cor=F)
lambda(model,-3,3)
model.box<- lm(log(Salary)~.,data=X)
#Validación box-cox
validaciongrafica(model.box)
###################
Hipo<-matrix(0,3,3)
Hipo[,1]<- c('Modelo/Valores P','Modelo','Modelo Box-Cox')

Hipo[,2]<- c('Shapiro Wilk(p-value)',0.00000000000005608,0.00002591)
Hipo[,3]<- c('Breusch Pagan(p-value)',0.0459,0.4471)
Hipo<-as.data.frame(Hipo)
colnames(Hipo)<- Hipo[1,]
rownames(Hipo)<- Hipo[,1]
Hipo<- Hipo[,-1]
Hipo<- Hipo[-1,]
Hipo
xtable(Hipo)
xtable(model.box)
# Ponderados
par(mfrow=c(2,2))
plot(fitted.values(model),studres(model),panel.first=grid(),pch=19,ylab='Residuos Estudentizados',xlab='Valores ajustados',main='A',col='aquamarine4')
lines(lowess(studres(model)~fitted.values(model)), col = "red1")
abline(h=c(-2,0,2),lty=2)
qqPlot(model,pch=19,ylab='Residuos Estudentizados',xlab='Cuantiles Teóricos',col=carPalette()[1],col.lines=carPalette()[3],main='B')
plot(fitted.values(model.box),studres(model.box),panel.first=grid(),pch=19,ylab='Residuos Estudentizados',xlab='Valores ajustados',main='C',col='aquamarine4')
lines(lowess(studres(model)~fitted.values(model.box)), col = "red1")
abline(h=c(-2,0,2),lty=2)
qqPlot(model.box,pch=19,ylab='Residuos Estudentizados',xlab='Cuantiles Teóricos',col=carPalette()[1],col.lines=carPalette()[3],main='D')
###### Minimos cuadrados ponderados
res.mcp<- residuals(model)
varianza<- lm(abs(res.mcp)~.,data=X)
w = 1/(fitted.values(varianza)^2)
model.ponderados<- lm(Salary~.,data=X,weights = w)
validacionmcp(model.ponderados)
summary(model.ponderados)
summary(model.box)
# Trabajar con el modelo con box-cox
#Multicolinealidad
car::vif(model.box)
K = seq(from=0,to=1,length.out = 100)
ridgesalary = lmridge(log(Salary)~., data=X,K=K,scaling='sc')
#####
criterios<- kest(ridgesalary)
par(mfrow=c(1,2))
plot(K,criterios$GCV,panel.first=grid(),type='l',xlab='K',ylab='validación cruzada',main='GCV')
points(K[criterios$GCV==min(criterios$GCV)],criterios$GCV[criterios$GCV==min(criterios$GCV)],pch=19,col='red1')
text(K[criterios$GCV==min(criterios$GCV)],criterios$GCV[criterios$GCV==min(criterios$GCV)],labels='0.161 ',pos=3)
##########
plot(K,criterios$CV,panel.first=grid(),type='l',xlab='K',ylab='validación cruzada',main='CV')
points(K[criterios$CV==min(criterios$CV)],criterios$CV[criterios$CV==min(criterios$CV)],pch=19,col='red1')
text(K[criterios$CV==min(criterios$CV)],criterios$CV[criterios$CV==min(criterios$CV)],labels='0.252 ',pos=3)
###########
lambda<-c(K[criterios$GCV==min(criterios$GCV)],K[criterios$CV==min(criterios$CV)])
lambda
######
ridgesalary<-lmridge(log(Salary)~., data=X,K=lambda[1],scaling='sc')
summary(ridgesalary)
summary(model.box)   
BIC(model.box)
AIC(model.box)
##### Regresión componentes principales
fit<- pcr(log(Salary)~.,data=X,scale=T,validation='CV')
varcomp(X[,-1])
############### Regresión por componentes principales
X. <- X[,-1] #Matriz de covariables
y<- X[,1] # Variable regresora escalonada
L<-PCR(X.,log(y),5)
# R cuadrado de 0.488
summary(ridgesalary)
summary(model.box)
#Comprobación
###############
escalar <- function(x) {(x-mean(x)) / sqrt(sum((x-mean(x))^2))}
y.e = escalar(log(X$Salary))
Z. = apply(X[,-1],2,escalar)
T.mat = eigen(t(Z)%*%Z)$vectors
lambda = eigen(t(Z)%*%Z)$values
P = Z%*%T.mat[,-(6:8)]
PCR.salary = lm(y.e~P-1)
###################### Rectificación del procedimiento
sigma2.pc = sum(PCR.salary$residuals^2)/(nrow(X)-8)
Var.b = sigma2.pc*eigen(cor(Z))$vectors%*%diag(c(1/eigen(cor(Z))$values[1:5],0,0,0))%*%t(eigen(cor(Z))$vectors)
summary(PCR.salary)
beta.CP =T.mat%*%c(PCR.salary$coefficients,0,0,0)
summary(ridgesalary)
summary(model.box)
head(predict(model,X))
head(exp(predict(model.box,X,interval='confidence')))
head(exp(predict(ridgesalary,X,interval='confidence')))
head(exp(predict(fit,X,ncomp=5,interval='confidence')))
########################