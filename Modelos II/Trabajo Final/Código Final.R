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
str(X)
attach(X)
#Modelo inicial
model1<- lm(Life.expectancy~.,data=X)
validaciongrafica(model1)
influencePlot(model1)
influence.measures(model1)
model2<- lm(Life.expectancy~.,data=X[-(which((rownames(X)=="Mozambique"))),])
validaciongrafica(model2)
influencePlot(model2)
X.<-X[-(which((rownames(X)=="Mozambique"))),]
N = nrow(X.); id = 1:N; N.tr=ceiling((1-0.324)*N)
set.seed(10); id.tr=sample(id,N.tr,replace=F)
X.tr= X.[id.tr,]   # Datos de entrenamiento de los modelos
X.te= X.[-id.tr,]
# Ajuste de los modelos
model<- lm(Life.expectancy~.,data=X.tr)
validaciongrafica(model)
summary(model)
car::vif(model)
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
validaciongrafica(modlasso)
influencePlot(modlasso)
car::vif(modlasso)
#Stepwise
step<-ols_step_both_aic(model,details = F)
modelstep<- lm(Life.expectancy~Infant.deaths+GDP+HIV.AIDS+Schooling+HDI+Status+Percentage.expenditure+Alcohol+Unemployment.rate..women,data=X.tr)
summary(modelstep)
car::vif(modelstep)
validaciongrafica(modelstep)
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
hist(Y$burg09,xlab="Frecuencia",ylab="Número de robos",main="",col="aquamarine4")
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "azure1")
hist(Y$burg09,xlab="Frecuencia",ylab="Número de robos",main="",col="aquamarine4",add=T)
##
boxplot(Y$burg09~Y$region)
names(Y)
poisson<- glm(burg09 ~ region + pct.male + sat.tot+act.comp+tuition+offset(log(total)), 
                     family = poisson(link = "log"), data = Y)
summary(poisson)
X2.poisson = sum(residuals(poisson,type='pearson')^2)
X2.poisson/poisson$df.residual
library(MASS)
library(pscl)
modbinNeg = glm.nb(burg09 ~ region + pct.male + sat.tot+act.comp+ tuition +offset(log(total)),data = Y)
summary(modbinNeg)
