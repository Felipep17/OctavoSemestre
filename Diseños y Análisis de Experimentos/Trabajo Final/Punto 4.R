library(readxl)
library(xtable)
CuartoPunto <- read_excel("C:/Users/sebas/OneDrive/Escritorio/datos_detergente_lavadora.xlsx")
View(CuartoPunto)
#Cuarto punto
X<- CuartoPunto
X$Lavadora<- factor(X$Lavadora)
boxplot(X$Blancura~X$Detergente)
model<- lm(Blancura~Detergente+ Lavadora,data=X)
xtable(anova(model))
car::Anova(model)
summary(model)
############
H<-rbind(mean(X$Blancura[X$Detergente=="A"])+ c(0,1,-1)*qt(0.025,(2)*(3))*sqrt(1.31)/sqrt(4),
mean(X$Blancura[X$Detergente=="B"])+c(0,1,-1)*qt(0.025,(2)*(3))*sqrt(1.31)/sqrt(4),
mean(X$Blancura[X$Detergente=="C"])+c(0,1,-1)*qt(0.025,(2)*(3))*sqrt(1.31)/sqrt(4),
mean(X$Blancura[X$Detergente=="D"])+c(0,1,-1)*qt(0.025,(2)*(3))*sqrt(1.31)/sqrt(4))
rownames(H)<- c("Detergente A","Detergente B","Detergente C","Detergente D")
colnames(H)<- c("Estimación Puntual","2.5%","97.5%")
xtable(H)
View(H)
