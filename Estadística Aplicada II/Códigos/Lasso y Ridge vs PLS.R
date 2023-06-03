library(plsdepot)
data("cornell")
YX <- cornell
View(YX)
#Multicolinealidad
library(lmridge)
K = seq(from=0,to=1,length.out = 100)
ridgesalary = lmridge(Octane~., data=YX[,-7],K=K,scaling='sc')
lambda<-c(K[criterios$GCV==min(criterios$GCV)],K[criterios$CV==min(criterios$CV)])
lambda
#####
criterios<- kest(ridgesalary)
par(mfrow=c(1,2))
plot(K,criterios$GCV,panel.first=grid(),type='l',xlab='K',ylab='validación cruzada',main='GCV')
points(K[criterios$GCV==min(criterios$GCV)],criterios$GCV[criterios$GCV==min(criterios$GCV)],pch=19,col='red1')
text(K[criterios$GCV==min(criterios$GCV)],criterios$GCV[criterios$GCV==min(criterios$GCV)],labels=lambda[1],pos=3)
##########
plot(K,criterios$CV,panel.first=grid(),type='l',xlab='K',ylab='validación cruzada',main='CV')
points(K[criterios$CV==min(criterios$CV)],criterios$CV[criterios$CV==min(criterios$CV)],pch=19,col='red1')
text(K[criterios$CV==min(criterios$CV)],criterios$CV[criterios$CV==min(criterios$CV)],labels=lambda[2],pos=3)
###########
######
ridgesalary<-lmridge(Octane~., data=YX[,-7],K=lambda[1],scaling='sc')
summary(ridgesalary)
vif.lmridge(ridgesalary)
# Lassso
#
library(glmnet)
model<- lm(Octane~.,data=YX)
X. = model.matrix(model)[,-1]
lasso.mod1 <- glmnet(X., YX$Octane,alpha = 1,nlambda = 10000)
plot(lasso.mod1,xvar='lambda',label=T,lwd=2,ylab='coeficientes de regresión')
abline(h=0,lty=2)
# CV
lasso.cv <-cv.glmnet(X., YX$Octane,nfolds = 4, alpha = 1,nlambda = 100)
plot(lasso.cv)
est = glmnet(X., YX$Octane, alpha = 1,lambda = lasso.cv$lambda.1se)
est$beta
modellasso<- lm(Octane~Distillation+Alkylat+NatEssence,data=YX)
summary(modellasso)
car::vif(modellasso)
library(ggfortify)
autoplot(modellasso)
shapiro.test(MASS::studres(modellasso))
lmtest::bptest(modellasso)
