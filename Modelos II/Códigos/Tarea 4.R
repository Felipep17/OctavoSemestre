X<- AbusoDrogas
names(X)
X<- X[,-1]
pairs(X)
str(X)
options(scipen=999)
modrugs.logit = glm(DFREE~AGE+BECK+NDRUGTX+TREAT,family=binomial(logit),data=X)
modrugs.probit = glm(DFREE~AGE+BECK+NDRUGTX+TREAT,family=binomial(probit),data=X)
modrugs.cloglog = glm(DFREE~AGE+BECK+NDRUGTX+TREAT,family=binomial(cloglog),data=X)
summary(modrugs.logit)
summary(modrugs.probit)
summary(modrugs.cloglog)
deviance(modrugs.logit)
deviance(modrugs.probit)
deviance(modrugs.cloglog)
library(pROC)
ROCbw.logit = roc(X$DFREE~modrugs.logit$fitted.values)
ROCbw.probit = roc(X$DFREE~modrugs.probit$fitted.values)
ROCbw.loglog = roc(X$DFREE~modrugs.cloglog$fitted.values)
plot(ROCbw.logit)
lines(ROCbw.probit,col=2)
lines(ROCbw.loglog,col=3)
#####
library(pROC)
# Cálculo de la curva ROC y determinación del mejor punto de corte usando el método de Youden
ROCbw.logit <- roc(X$DFREE, modrugs.logit$fitted.values)
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

