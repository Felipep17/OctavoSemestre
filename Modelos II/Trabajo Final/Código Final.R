library(readr)
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Modelos II/Trabajo Final")
library(easypackages)
lib_req<-c("glmnet","lmridge","scatterplot3d","plot3D","plotly","rgl","plot3Drgl",
           'effects','psych',
           'car','lmtest',"nortest",'MASS','latex2exp','orcutt',
           'nlme',"zoom",'ggfortify','readxl','pls',"alr4","aod","mixtools","ddalpha","olsrr")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)
X<- data.frame(read_csv("WHO2016.csv"))
source("Source.R")
rownames(X)<-X[,1];X<- X[,-1]
X[,1]<- factor(X[,1])
X[,10]<- X[,10]+1
X[,10:11]<- log(X[,10:11])
colnames(X)[c(10,11)]<-c("Log(HIV.AIDS)","Log(GDP)")
X<- X[,-13]
colnames(X)
plot(X)
View(X)
str(X)
Y<- cbind(X[,2],X[,-2])
colnames(Y)[1]<- "Life.expetancy"
par(mar=c(4,4,1,1))
par(mfrow=c(4,4))
options(scipen=999)
col<- ifelse(Y[,2]=="Developed",1,2)+2
sapply(seq(2,ncol(Y)),function(j)plot(Y[,1]~Y[,j],xlab=paste(colnames(Y)[j]),ylab=colnames(Y)[1],col=col,pch=19))
sapply(seq(3,ncol(Y)),function(j)plot(density(Y[,j]),xlab=paste(colnames(Y)[j]),ylab="Densidad",col="blue",main="",lwd=2))
boxplot(Life.expectancy~X[,1],xlab="Status",ylab="Life.expentacy")
attach(X)
#Modelo inicial
summary(X)
model1<- lm(Life.expectancy~.,data=X)
validaciongrafica(model1)
influencePlot(model1)
influence.measures(model1)
model2<- lm(Life.expectancy~.,data=X)
validaciongrafica(model2)
influencePlot(model2)
X.<-X
View(X.)
N = nrow(X.); id = 1:N; N.tr=ceiling((1-0.324)*N)
set.seed(10); id.tr=sample(id,N.tr,replace=F)
X.tr= X.[id.tr,]   # Datos de entrenamiento de los modelos
X.te= X.[-id.tr,]
# Ajuste de los modelos
model<- lm(Life.expectancy~.,data=X.tr)
library(xtable)
xtable(summary(model))
validaciongrafica(model)
summary(model)
xtable(t(t(as.matrix(car::vif(model)))))
#Ajuste por medio de LASSO
library(glmnet)
options(scipen = 999)
XD = model.matrix(model)[,-1]
lasso.mod1 <- glmnet(XD, X.tr$Life.expectancy,alpha = 1,nlambda = 10000)
plot(lasso.mod1,xvar='lambda',label=T,lwd=2,ylab='coeficientes de regresión')
abline(h=0,lty=2)
# CV
lasso.cv <-cv.glmnet(XD,X.tr$Life.expectancy,nfolds = 10, alpha = 1,nlambda = 100)
plot(lasso.cv)
est = glmnet(XD, X.tr$Life.expectancy, alpha = 1,lambda = lasso.cv$lambda.1se)
ind<-which(est$beta==0)
modlasso<- lm(Life.expectancy~.,data=X.tr[,-(ind+1)])
summary(modlasso)
xtable(modlasso)
validaciongrafica(modlasso)
influencePlot(modlasso)
xtable(t(t(car::vif(modlasso))))
#Stepwise
step<-ols_step_both_aic(model,details = F)
modelstep<- lm(Life.expectancy~Infant.deaths+`Log(GDP)`+`Log(HIV.AIDS)`+Status +Schooling  +Alcohol+Percentage.expenditure+BMI,data=X.tr)
xtable(summary(modelstep))
summary(modelstep)
car::vif(modelstep)
xtable(modlasso)
xtable(modelstep)
validaciongrafica(modelstep)
xtable(t(t(car::vif(modelstep))))
xtable(t(vif(modlasso)[7:12]))
AIC(model)
AIC(modlasso)
AIC(modelstep)
AIC(model2)
summary(modelstep)
summary(modlasso)
#Validación
pre1<- predict(modlasso,X.te)
ecmp1<- (1/48)*sum(X.te$Life.expectancy-pre1)^2
pre2<- predict(modelstep,X.te)
ecmp2<- (1/48)*sum(X.te$Life.expectancy-pre2)^2
####### Punto 2
library(readr)
Y <- data.frame(read_csv("campuscrime.csv"))
View(Y)
rownames(Y)<- Y[,2]
Y<- Y[,-(1:2)]
hist(Y$burg09,xlab="Frecuencia",ylab="Número de robos",main="",col="aquamarine4",breaks=10)
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "azure1")
hist(Y$burg09,xlab="Frecuencia",ylab="Número de robos",main="",col="aquamarine4",add=T,breaks=10)
     
##
boxplot(Y$burg09~Y$region,xlab="Región",ylab="Número de robos",col="aquamarine4")
# Points
stripchart(Y$burg09~Y$region,              # Data
           method = "jitter" , pch=19,         # Pch symbols
           col = 'black',           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   
grid()
names(Y)
poisson<- glm(burg09 ~ region + pct.male + sat.tot+act.comp+tuition+offset(log(total)), 
                     family = poisson(link = "log"), data = Y)
summary(poisson)
X2.poisson = sum(residuals(poisson,type='pearson')^2)
X2.poisson/poisson$df.residual
library(MASS)
library(pscl)
modbinNeg = glm.nb(burg09 ~ region + pct.male + sat.tot+act.comp+ tuition +offset(log(total)),data = Y)
xtable(summary(modbinNeg))
X2.poisson1 = sum(residuals(modbinNeg,type='pearson')^2)
si2<-X2.poisson1/modbinNeg$df.residual
si1<-X2.poisson/poisson$df.residual
AIC(poisson)
AIC(modbinNeg)
H<-matrix(c(si1,AIC(poisson),deviance(poisson),si2,AIC(modbinNeg),deviance(modbinNeg)),nrow=2,ncol=3)
colnames(H)<- c("Psi","AIC","Devianza")
rownames(H)<- c("Modelo Poisson Ofsset","Modelo Binomial Negativo")
xtable(H)
##########
Modelo_PCA <- FactoMineR::PCA(X[,-1],ncp=2,quali.sup=8)
H<-matrix(X[,-1] )
windows(height=10,width=15)
factoextra::fviz_pca_ind(H,addEllipses = T, ellipse.level = 0.95) 
windows(height=10,width=15)
color=c("Gray","Blue")  
factoextra::fviz_pca_biplot(Modelo_PCA,col.ind=color) 
