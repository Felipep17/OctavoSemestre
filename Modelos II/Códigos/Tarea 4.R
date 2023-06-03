library(xtable)
library(aod)
X<- AbusoDrogas
names(X)
X<- X[,-1]
pairs(X)
str(X)
options(scipen=999)
modrugs.logit = glm(DFREE~AGE+BECK+NDRUGTX+TREAT,family=binomial(logit),data=X)
p<- c()
for(i in 1:length(coef(modrugs.logit))){
p[i]<- exp(coef(modrugs.logit)[i])
}
cbind(coef(modrugs.logit),p)[-1,]
xtable(cbind(coef(modrugs.logit),p)[-1,])
xtable(summary(modrugs.logit))
modrugs.logitinter = glm(DFREE~AGE*TREAT+BECK*TREAT+NDRUGTX*TREAT,family=binomial(logit),data=X)
xtable(anova(modrugs.logit,modrugs.logitinter,test='LRT'))
VarMat = vcov(modrugs.logitinter)
Coef = coefficients(modrugs.logitinter)
xtable(summary(modrugs.logitinter))
wald.test(VarMat,Coef,Terms=c(6,7,8))
summary(modrugs.logit)
summary(modrugs.logitinter)
car::vif(modrugs.logit)
modrugs.probit = glm(DFREE~AGE+BECK+NDRUGTX+TREAT,family=binomial(probit),data=X)
modrugs.cloglog = glm(DFREE~AGE+BECK+NDRUGTX+TREAT,family=binomial(cloglog),data=X)
Mods<-round(cbind(coef(modrugs.logit),sqrt(diag(vcov(modrugs.logit))),coef(modrugs.logit)/sqrt(diag(vcov(modrugs.logit))),(1-pnorm(abs(coef(modrugs.logit)/sqrt(diag(vcov(modrugs.logit))))))*2,coef(modrugs.probit),sqrt(diag(vcov(modrugs.probit))),coef(modrugs.probit)/sqrt(diag(vcov(modrugs.probit))),(1-pnorm(abs(coef(modrugs.probit)/sqrt(diag(vcov(modrugs.probit))))))*2,coef(modrugs.cloglog),sqrt(diag(vcov(modrugs.cloglog))),coef(modrugs.cloglog)/sqrt(diag(vcov(modrugs.cloglog))),(1-pnorm(abs(coef(modrugs.cloglog)/sqrt(diag(vcov(modrugs.cloglog))))))*2),4)
rbind(0,0,Mods)
name<-c("Est","Std","Z", "P-Val")
Mods[1,]<-c(name,name,name)

crit<- cbind(rbind(AIC(modrugs.logit),deviance(modrugs.logit)),rbind(AIC(modrugs.probit),deviance(modrugs.probit)),rbind(AIC(modrugs.cloglog),deviance(modrugs.cloglog)))
colnames(crit)<- c("Logit","Probit","CLogLog")
rownames(crit)<- c("AIC","Devianza")
xtable(crit)
xtable(Mods)
summary(modrugs.cloglog)
y<-c()
x<-coef(modrugs.probit)/sqrt(diag(vcov(modrugs.probit)))
for( i in 1:length(x)){
  y[i]<- (1-pnorm(abs(x[i])))*2
  
}

(1-pnorm(1.24))*2
summary(modrugs.probit)
summary(modrugs.cloglog)
deviance(modrugs.logit)
deviance(modrugs.probit)
deviance(modrugs.cloglog)
library(pROC)
ROCbw.logit = roc(X$DFREE~modrugs.logit$fitted.values)
ROCbw.probit = roc(X$DFREE~modrugs.probit$fitted.values)
ROCbw.loglog = roc(X$DFREE~modrugs.cloglog$fitted.values)
x11()
?roc
plot(ROCbw.logit, print.thres = "best", print.auc = TRUE,
     auc.polygon = FALSE, max.auc.polygon = F, auc.polygon.col = "gainsboro",
     col = 1, grid = TRUE,xlim=c(1,0))
metricas<- cbind(rbind(0.633,0.453,0.776,0.235),rbind(0.631,0.435,0.782,0.233),rbind(0.633,0.465,0.776,0.235))
colnames(metricas)<- c("Logit","Probit","CLogLog")
rownames(metricas)<- c("AUC","Sensibilidad","Especificidad","Punto de corte")
xtable(metricas)
ROCbw.logit

lines(ROCbw.probit,col=2, print.thres = "best", print.auc = TRUE,
      auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro"
      , grid = TRUE)
lines(ROCbw.loglog,col=3, print.thres = "best", print.auc = TRUE,
      auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro"
      , grid = TRUE)
legend(x = "topleft",legend=c("Logit","Probit","CLogLog"),
       col=c(1,2,3),lwd=2,title='Enlace',
       box.lwd=1,text.font =20,cex=0.8)
table(X$DFREE)
#####
library(pROC)
# Cálculo de la curva ROC y determinación del mejor punto de corte usando el método de Youden
ROCbw.logit <- roc(X$DFREE, modrugs.logit$fitted.values)
coords(ROCbw.logit, "best")
cutoff_youden_logit <- coords(ROCbw.logit, "best", best.method = "youden")
sensitivity_youden_logit <- cutoff_youden_logit$sensitivity
specificity_youden_logit <- cutoff_youden_logit$specificity

# Cálculo de la curva ROC y determinación del mejor punto de corte usando el método de closest.topleft
ROCbw.probit <- roc(X$DFREE, modrugs.logit$fitted.values)
cutoff_closest_logit <- coords(ROCbw.logit, "best", best.method = "closest.topleft")
sensitivity_closest_logit <- cutoff_closest_logit$sensitivity
specificity_closest_logit <- cutoff_closest_logit$specificity

# Imprimir los resultados
cat("Método de Youden (Logit): \n")
cat("Mejor punto de corte:", cutoff_youden_logit$threshold, "\n")
cat("Sensibilidad:", sensitivity_youden_logit, "\n")
cat("Especificidad:", specificity_youden_logit, "\n")

cat("\nMétodo de closest.topleft (Logit): \n")
cat("Mejor punto de corte:", cutoff_closest_logit$threshold, "\n")
cat("Sensibilidad:", sensitivity_closest_logit, "\n")
cat("Especificidad:", specificity_closest_logit, "\n")
# Obtener las predicciones utilizando el mejor punto de corte según el método de Youden
predicciones_youden_logit <- ifelse(modrugs.logit$fitted.values >= cutoff_youden_logit$threshold, 1, 0)

# Calcular la matriz de confusión
matriz_confusion_youden_logit <- table(Real = X$DFREE, Prediccion = predicciones_youden_logit)
print(matriz_confusion_youden_logit)
# Obtener las predicciones utilizando el mejor punto de corte según el método de closest.topleft
predicciones_closest_logit <- ifelse(modrugs.logit$fitted.values >= cutoff_closest_logit$threshold, 1, 0)

# Calcular la matriz de confusión
matriz_confusion_closest_logit <- table(Real = X$DFREE, Prediccion = predicciones_closest_logit)
print(matriz_confusion_closest_logit)
#Sobre dispersión
phi<-rbind(sum(residuals(modrugs.logit,method="pearson")^2)/modrugs.logit$df.residual,
                    sum(residuals(modrugs.probit,method="pearson")^2)/modrugs.probit$df.residual,
                    sum(residuals(modrugs.cloglog,method="pearson")^2)/modrugs.cloglog$df.residual)
rownames(phi)<- c("Logit","Probit","CLogLog")
colnames(phi)<- "Sobredispersión"
xtable(phi)
#Bondad de ajuste
# devianza
D = deviance(modrugs.logit)
1-pchisq(D,6) # valor p
# chi-cuadrado de Pearson
X2 = sum(residuals(modrugs.logit,type='pearson')^2)
1-pchisq(X2,6) # valor p
#Hipotesis
modrugs.logit = glm(DFREE~AGE+BECK+NDRUGTX+TREAT,family=binomial(logit),data=X)
modrugs.logit2 = glm(DFREE~AGE+NDRUGTX+TREAT,family=binomial(logit),data=X)
summary(modrugs.logit)
summary(modrugs.logit2)
anova(modrugs.logit,modrugs.logit2,test='LRT')
#
plot(fitted.values(modrugs.logit2))
abline(h=0.235)
points(fitted.values(modrugs.logit2)[modrugs.logit2$fitted.values>0.235],pch=19,col=2)
points(fitted.values(modrugs.logit2)[modrugs.logit2$fitted.values<=0.235],pch=19,col=4)
library(pROC)
ROCbw.logit = roc(X$DFREE~modrugs.logit$fitted.values)
plot(ROCbw.logit, print.thres = "best", print.auc = TRUE,
     auc.polygon = FALSE, max.auc.polygon = F, auc.polygon.col = "gainsboro",
     col = 2, grid = TRUE,xlim=c(1,0))
cutoff_youden_logit <- coords(ROCbw.logit, "best")
sensitivity_youden_logit <- cutoff_youden_logit$sensitivity
specificity_youden_logit <- cutoff_youden_logit$specificity

# Obtener las predicciones utilizando el mejor punto de corte según el método de Youden
predicciones_youden_logit <- ifelse(modrugs.logit$fitted.values >= cutoff_youden_logit$threshold, 1, 0)

# Calcular la matriz de confusión
matriz_confusion_youden_logit <- table(Real = X$DFREE, Prediccion = predicciones_youden_logit)
print(matriz_confusion_youden_logit)
# Imprimir los resultados
cat("Método de Youden (Logit): \n")
cat("Mejor punto de corte:", cutoff_youden_logit$threshold, "\n")
cat("Sensibilidad:", sensitivity_youden_logit, "\n")
cat("Especificidad:", specificity_youden_logit, "\n")
