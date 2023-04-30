##############################
options(scipen=999)
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Minería de Datos/Laboratorios")
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
  plot(1:length(lambdas),prop.var,type='l',panel.first=grid(),ylim=c(0,1),xlab='Componente principal',ylab='Porcentaje acumulado variabilidad explicada')
  text(1:length(lambdas),prop.var,labels=as.character(paste(round(prop.var.accum*100,2),'%')),cex=0.8,pos=c(4,4,1,3,3,3,3,3))
  points(1:length(lambdas),prop.var,pch=19,col=c('aquamarine4'))
  lambdas<- t(as.data.frame(lambdas))
  return(lambdas)
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
    t.value<- beta.CP1/sqrt(diag(vbb))
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
    t.value<- beta.CP1/sqrt(diag(vbb))
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
ind<- c(19,1,8,2,9,4,11,3,10,5,12,18,17,6,13,7)
exploratorio(X[,ind])
X<- X[,ind]
plot(X,panel.first=grid(),col='aquamarine4')
names(X)
model<- lm(Salary~.,data=X)
summary(model)
car::vif(model)
# Validación de supuestos
validaciongrafica(model,cor=F)
lambda(model,-3,3)
model.box<- lm(I(Salary^0.11)~.,data=X)
vif<-as.data.frame(t(as.numeric(car::vif(model.box))))
names(X)
colnames(vif)<- colnames(X[,-1])
xtable(vif)
#Validación box-cox
validaciongrafica(model.box)
###################
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
summary(model)
# Trabajar con el modelo con box-cox
par(mfrow=c(1,2))
hist(residuals(model.box),freq=F,xlab='',main='Box-Cox')
lines(density(residuals(model.box)))
hist(residuals(model.ponderados)*sqrt(weights(model.ponderados)),freq=F,xlab='',main='MCP')
lines(density((residuals(model.ponderados)*sqrt(weights(model.ponderados)))))
###############
library(olsrr)
ols_step_best_subset(model.ponderados)
ols_step_best_subset(model.box)
ols_step_forward_aic(model.box,details = F)
ols_step_backward_aic(model.box,details = F)
ols_step_both_aic(model.box,details = F)
ols_step_forward_aic(model.ponderados,details = F)
ols_step_backward_aic(model.ponderados,details = F)
#Box-Cox
#
library(glmnet)
X. = model.matrix(model)[,-1]
lasso.mod1 <- glmnet(X., X$Salary,alpha = 1,nlambda = 10000)
plot(lasso.mod1,xvar='lambda',label=T,lwd=2,ylab='coeficientes de regresión')
abline(h=0,lty=2)
# CV
lasso.cv <-cv.glmnet(X., X$Salary,nfolds = 10, alpha = 1,nlambda = 100)
plot(lasso.cv)
est = glmnet(X., X$Salary, alpha = 1,lambda = lasso.cv$lambda.1se)
est$beta
modellasso<- lm(log(Salary)~Hits+CRuns+CRBI+Walks,data=X)
summary(modellasso)
car::vif(modellasso)
modelstep<- lm(I(Salary^0.11)~CRuns+Hits+Years+Walks+AtBat+CWalks,data=X)
summary(modelstep)
car::vif(modelstep)
validaciongrafica(modelstep)
validaciongrafica(modellasso)
vif<-as.data.frame(t(as.numeric(car::vif(model.box))))
xtable(as.data.frame(t(as.numeric(car::vif(modellasso)))))
backward<- lm(Salary~CRuns+Hits+Years+Walks+AtBat+CWalks,data=X)
r2<-lm(Salary~CRuns+Hits+Years+Walks+AtBat+CWalks+HmRun,data=X)
xtable(as.data.frame(t(as.numeric(car::vif(backward)))))
plot(rnorm(1000),rnorm(1000))
xtable(as.data.frame(t(as.numeric(car::vif(r2)))))
