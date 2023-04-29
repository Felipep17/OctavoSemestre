##############################
options(scipen=999)
#
library(lmridge)
library(easypackages)
library(glmnet)
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Estadística Aplicada II/Base de datos")
lib_req<-c("glmnet","lmridge","scatterplot3d","plot3D","plotly","rgl","plot3Drgl",'effects','psych','car','lmtest','MASS','xtable','latex2exp','orcutt','nlme',
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
summary(model.box)
#Selección de variables
attach(X)
X.<-model.matrix(lm(density~.,data=X))[,-1]
lasso.mod <- glmnet(X., Y., alpha = 1,nlambda = 100)
lasso.mod$beta
plot(lasso.mod,xvar='lambda',label=T,lwd=2,ylab='coeficientes de regresión')
abline(h=0,lty=2)
lasso.cv <-cv.glmnet(X., Y., nfolds = 4, alpha = 1,nlambda = 100)
plot(lasso.cv)
est = glmnet(X., Y., alpha = 1,lambda = lasso.cv$lambda.1se)
est$beta
model.lasso<- lm(density~NIR1+NIR6+NIR18+NIR28+NIR29,data=X)
model.lasso1<- lm(density~NIR1+NIR6+NIR18+NIR28,data=X)
validaciongrafica(model.lasso1)
anova(model.lasso,model.lasso1)
summary(model.lasso1)
car::vif(model.lasso1)
# Regresión ridge
K = seq(from=0,to=1,length.out = 100000)
ridgesalary = lmridge(density~NIR1+NIR6+NIR18+NIR28,data=X,K=K,scaling='sc')
#####
criterios<- kest(ridgesalary)
par(mfrow=c(1,2))
plot(K,criterios$GCV,panel.first=grid(),type='l',xlab='K',ylab='validación cruzada',main='GCV')
points(K[criterios$GCV==min(criterios$GCV)],criterios$GCV[criterios$GCV==min(criterios$GCV)],pch=19,col='red1')
text(K[criterios$GCV==min(criterios$GCV)],criterios$GCV[criterios$GCV==min(criterios$GCV)],labels=paste(K[1]),pos=3)
##########
plot(K,criterios$CV,panel.first=grid(),type='l',xlab='K',ylab='validación cruzada',main='CV')
points(K[criterios$CV==min(criterios$CV)],criterios$CV[criterios$CV==min(criterios$CV)],pch=19,col='red1')
text(K[criterios$CV==min(criterios$CV)],criterios$CV[criterios$CV==min(criterios$CV)],labels=paste(K[2]),pos=3)
###########
lambda<-c(K[criterios$GCV==min(criterios$GCV)],K[criterios$CV==min(criterios$CV)])
lambda
######
ridgesalary<-lmridge(I(density^0.65)~NIR1+NIR6+NIR18+NIR28, data=X,K=0.01,scaling='sc')
summary(ridgesalary)
vif.lmridge(ridgesalary)
car::vif(model.lasso1)
plot(fitted.values(ridgesalary),residuals(ridgesalary),pch=19)
abline(h=0,lty=2,lwd=2)
bptest(ridgesalary)
# Elastic NET
library(glmnet)   
library(faraway) 
library(caret)
#Intervalo para alpha y lambda: Creando Grid
search.grid <-expand.grid(alpha = seq(0,1,.1),
                          lambda = exp(seq(-1,1,.1)))

#Validacion cruzada
train.control <- trainControl(method = "cv", 
                              number = 10)
#Modelo
step.model <- train(I(density^0.65)~., 
                    data = X,
                    method = "glmnet", 
                    trControl = train.control,
                    tuneGrid = search.grid)

#Alpha y lambda optimos:
step.model$bestTune$alpha
step.model$bestTune$lambda
#Coeficientes 
coef(step.model$finalModel, step.model$bestTune$lambda)
elastic.model <- glmnet(x=X., y=I(Y.^0.65),
                        alpha  = 0.8, 
                        lambda =0.3678794)
elastic.model$beta
plot(I(density^0.65)-predict(elastic.model,X.),predict(elastic.model,X.),pch=19)
#Ejercicio de Predicción
predict(elastic.model, 
        newx = matrix(c(age=24, weight=210.25, height=74.75, 
                        adipos=26.5, neck=39.0, chest=104.5, 
                        abdom=94.4, hip=107.8, thigh=66.0, 
                        knee=42.0, ankle=25.6,  biceps=35.7, 
                        forearm=30.6, wrist=18.8),
                      nrow = 1)
)
influence.measures(model.box)
anova(model.lasso1)
