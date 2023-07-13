#install.packages("easypackages")        # Libreria especial para hacer carga automática de librerias
library("easypackages")
lib_req<-c("MASS","aod","doBy",
           "FactoMineR","factoextra","ggdist",
           "caret","pROC","xtable","ggplot2")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)  
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Modelos II/Bases de datos")
#Limpieza columna ineccesaria
X<- AbusoDrogas;View(X);names(X);X<- X[,-1]
X<- X[,c(8,1,2,4,6)]
View(X)
# Cargar la librería ggplot2
tabla1<-data.frame(prop.table(table(X$DFREE)))
colnames(tabla1)<- c("Categoria","Probabilidad")
tabla1$Probabilidad<- round(tabla1$Probabilidad,2);tabla1
# Crear el gráfico de barras con ggplot2 y personalizarlo
ggplot(tabla1, aes(x = Categoria, y = Probabilidad, fill = Categoria)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Probabilidad), vjust = -0.5) +
  scale_fill_manual(values = c("Free" = "red", "No Free"="aquamarine4"))+
  labs(title = "DrugsFree")
########### Tratamiento
tabla2<-data.frame(prop.table(table(X$TREAT)))
colnames(tabla2)<- c("Categoria","Probabilidad")
tabla2$Probabilidad<- round(tabla2$Probabilidad,2);tabla2
# Crear el gráfico de barras con ggplot2 y personalizarlo
ggplot(tabla2, aes(x = Categoria, y = Probabilidad, fill = Categoria)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Probabilidad), vjust = -0.5) +
  labs(title = "TREAT")
###############
tabla3<-data.frame(prop.table(table(X$NDRUGTX)))
colnames(tabla3)<- c("Categoria","Probabilidad")
tabla3$Probabilidad<- round(tabla3$Probabilidad,2);tabla3
# Crear el gráfico de barras con ggplot2 y personalizarlo
ggplot(tabla3, aes(x = Categoria, y = Probabilidad, fill = Categoria)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Probabilidad), vjust = -0.5) +
  labs(title = "NDRUGTX")
################### Cuantitaivas Descriptivas
par(mfrow=c(1,2))
hist(X$AGE,main="Age",freq=F,ylab="Frequency",xlab="Age",panel.first=grid(),col="aquamarine4",xlim=c(0,70))
lines(density(X$AGE),lwd=2,lty=2);summary(X$AGE)
hist(X$BECK,main="BECK",freq=F,ylab="Frequency",xlab="BECK",panel.first=grid(),col="purple1",xlim=c(0,70))
lines(density(X$BECK),lwd=2,lty=2);summary(X$BECK)
pairs(AGE~BECK,col=X$DFREE+1,pch=19,data=X)
pairs(AGE~BECK,col=X$TREAT+1,pch=19,data=X)
#######
#Notación científica
options(scipen=999)
#Modelo logístico
modrugs.logit <- glm(DFREE~AGE+BECK+NDRUGTX+TREAT,family=binomial(logit),data=X)
#Resumen
summary(modrugs.logit)
#OR
exp(coef(modrugs.logit))
# Gráfico
# Obtener los coeficientes, intervalos de confianza y valores p del modelo logístico
coeficientes <- coef(modrugs.logit)
intervalos_confianza <- confint(modrugs.logit)
valores_p <- summary(modrugs.logit)$coefficients[, "Pr(>|z|)"]

# Crear un data frame con los coeficientes, intervalos de confianza y valores p
datos_coeficientes <- data.frame(
  Variables = names(coeficientes),
  Coeficientes = coeficientes,
  Inferior_CI = intervalos_confianza[, 1],
  Superior_CI = intervalos_confianza[, 2],
  Valores_p = valores_p
)

# Crear el gráfico de barras con intervalos de confianza y etiquetas
grafico_coeficientes <- ggplot(datos_coeficientes, aes(x = Variables, y = Coeficientes)) +
  geom_pointrange(aes(ymin = Inferior_CI, ymax = Superior_CI), color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(Coeficientes, 2), " [", round(Inferior_CI, 2), "; ", round(Superior_CI, 2), "]\n(p = ", round(Valores_p, 3), ")")), 
            vjust = -1.5, color = "black") +
  coord_flip() +
  labs(x = "Variables", y = "Odds Ratio", title = "Odds del Modelo Logístico") +
  theme_minimal()

# Mostrar el gráfico
print(grafico_coeficientes)
ROCbw.logit = roc(X$DFREE~modrugs.logit$fitted.values)
plot(ROCbw.logit, print.thres = "best", print.auc = TRUE,
     auc.polygon = FALSE, max.auc.polygon = F, auc.polygon.col = "gainsboro",
     col = 1, grid = TRUE,xlim=c(1,0))
#Class logit
Class.Logit<-as.factor(ifelse(modrugs.logit$fitted.values<0.235,"No","Yes"))
Class.original<- as.factor(ifelse(X$DFREE==0,"No","Yes"))
############
caret::confusionMatrix(Class.Logit,Class.original,dnn=c("Drugs - Logit", "Drugs - Obs"),positive = "Yes",mode="everything")
