#------------------------------------------------------------------------------#
#                  Universidad del Valle - Escuela de Estadística              #
#                            Programa Académico de:                            #
#                             Pregrado en Estadística                           #
#  Asignatura : Técnicas de Minería de Datos y Aprendizaje Automático          #           
#                      Estadístico - Jaime Mosquera Restrepo                   #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#          0. Configuración inicial-Librerías requeridas                    ####
#------------------------------------------------------------------------------#
                            # Establecer el directorio de trabajo  

#install.packages("easypackages")        # Libreria especial para hacer carga automática de librerias
library("easypackages")
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Minería de Datos/Códigos")
lib_req<-c("readxl","visdat","corrplot","plotrix","cluster","factoextra", "FactoMineR")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)         # Verificación, instalación y carga de librerias.
source("Script R - source.R")

#------------------------------------------------------------------------------#
#   1. Aprendizaje no Supervisado - Reducción - Segmentación                ####
#              Caso -  PIB - Producción Económica Colombia                     #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
##                1.1 Lectura de Datos y ajuste estructura                  ####
#------------------------------------------------------------------------------#
PIB=data.frame(read_excel("PIBpc.xlsx",col_names=TRUE, range = "B2:O35"))

rownames(PIB)=PIB[,1]; PIB=PIB[,-1]
View(PIB); str(PIB)  
windows(height=10,width=15); visdat::vis_miss(PIB)            

#------------------------------------------------------------------------------#
##                1.2 Preprocesamiento de datos                             ####
#------------------------------------------------------------------------------#
coef_var=function(x){sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)}

Resumen= rbind(apply(PIB,2,"mean"),apply(PIB,2,"sd"),apply(PIB,2,"coef_var"))
rownames(Resumen)=c("Promedio", "Desviación","Coef. Var")
print(Resumen,2)
   
# Datos Atipicos univariados
windows(height=10,width=15)
par(mfrow=c(3,5))
lapply(colnames(PIB),function(y){
   boxplot(PIB[,y],ylab=y,cex=1.5,pch=20,col="blue")
})

Out.Uni=lapply(PIB,id.out.uni,method="Tukey")
rownames(PIB)[unlist(Out.Uni)]

## Análisis Bivariado de la correlación.

windows(height=10,width=15)
pairs(PIB,pch=20,cex=1.5,lower.panel = NULL)

M.cor = cor(PIB,method="pearson"); print(M.cor,2)
p.cor=corrplot::cor.mtest(PIB)$p; print(p.cor,4)

windows(height=10,width=15)
corrplot::corrplot(M.cor, method = "ellipse",addCoef.col = "black",type="upper",
                   col=c("blue","red"),diag=FALSE,
                   p.mat = p.cor, sig.level = 0.01, insig = "blank"
                   )
id.out.mult=out.mult(PIB)

# Sin considerar el registro "atípico "Bogota"

windows(height=10,width=15)
pairs(PIB[-3,],pch=20,cex=1.5,lower.panel = NULL)

M.cor = cor(PIB[-3,],method="pearson"); print(M.cor,2)
p.cor=corrplot::cor.mtest(PIB[-3,])$p; print(p.cor,4)

windows(height=10,width=15)
corrplot::corrplot(M.cor, method = "ellipse",addCoef.col = "black",type="upper",
                   col=c("blue","red"),diag=FALSE,
                   p.mat = p.cor, sig.level = 0.01, insig = "blank")


# Sin considerar los 3 registros atipicos multivariado
index.out=id.out.mult$Out_dist
windows(height=10,width=15)
pairs(PIB[-index.out,],pch=20,cex=1.5,lower.panel = NULL)

M.cor = cor(PIB[-index.out,],method="pearson"); print(M.cor,2)
p.cor=corrplot::cor.mtest(PIB[-index.out,])$p; print(p.cor,4)

windows(height=10,width=15)
corrplot::corrplot(M.cor, method = "ellipse",addCoef.col = "black",type="upper",
                   col=c("blue","red"),diag=FALSE,
                   p.mat = p.cor, sig.level = 0.01, insig = "blank")

#------------------------------------------------------------------------------#
##     2. Análisis de Componentes Principales                               ####
#------------------------------------------------------------------------------#
X=PIB[-index.out,]
PCA=PCA(X,scale.unit=TRUE,ncp=10,graph=FALSE)   # Las variables son autoescaladas

PCA
summary(PCA)

#------------------------------------------------------------------------------#
##     2.1 Selección e interpretación de componentes                        ####
#------------------------------------------------------------------------------#
PCA$eig
VP=PCA$eig[,1]; Var= PCA$eig[,2]; Var_acum=PCA$eig[,3]  # Valores propios y varianza acumulada

windows(height=10,width=15)
par(mfrow=c(1,2))
coord=barplot(VP, xlab="Componente",ylab="Valor Propio", ylim=c(0,max(VP)+1))
lines(coord,VP,col="blue",lwd=2)
text(coord,VP,paste(round(Var,2),"%"), pos=3,cex=0.6)
abline(h=1,col="red", lty=2)
coord=barplot(Var_acum, xlab="Componente",ylab="Varianza Acumulada")
lines(coord,Var_acum,col="blue",lwd=2)
text(coord,Var_acum,round(Var_acum,2), pos=3,cex=0.6)

# 3 componentes-->  82% de la Varianza Explicada --> Son interpretables?
PCA_var=get_pca_var(PCA)

PCA_var$coord[,1:3]; PCA_var$cos2[,1:3]; PCA_var$contrib[,1:3]

windows(height=10,width=15)
corrplot(PCA_var$cos2, is.corr=FALSE)

windows(height=10,width=15)
fviz_cos2(PCA, choice = "var", axes = 1:2)

windows(height=10,width=15)
fviz_cos2(PCA, choice = "var", axes = 1:3)

# Interpretar las componentes a través de sus coeficientes
windows(height=10,width=15)
par(mfrow=c(3,1))
barplot(PCA_var$coord[,1],ylim=c(-0.8,0.8),col=ifelse(PCA_var$coord[,1]>0,"green","red"),
        main="Coeficientes estimados PC1")
barplot(PCA_var$coord[,2],ylim=c(-0.8,0.8),col=ifelse(PCA_var$coord[,2]>0,"green","red"),
        main="Coeficientes estimados PC2")
barplot(PCA_var$coord[,3],ylim=c(-0.8,0.8),col=ifelse(PCA_var$coord[,3]>0,"green","red"),
        main="Coeficientes estimados PC3")

# Visualización de la Correlación entre variables
windows(height=10,width=15)
fviz_pca_var(PCA,axes=c(1,2), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE )
windows(height=10,width=15)
fviz_pca_var(PCA,axes=c(1,3),  col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
windows(height=10,width=15)
fviz_pca_var(PCA,axes=c(2,3), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

#------------------------------------------------------------------------------#
##     2.2 Visualización de individuos en el espacio de las componentes     ####
#------------------------------------------------------------------------------#
PCA_ind=get_pca_ind(PCA)

##  Representar los Departamentos en las componentes principales

Sector_23 = PCA_ind$coord[,1]
Sector_1 = PCA_ind$coord[,2]
Sector_Soc = PCA_ind$coord[,3]

windows(height=10,width=15)
par(mfrow=c(1,3))
dotchart(Sector_23,labels=rownames(X),pch=20,cex.lab=0.5, main= "PC1 : Sector_23",
         cex.lab=0.8)
abline(v=0,col="red",lty=2)
dotchart(Sector_1,pch=20,labels=rownames(X), main= "PC2 : Sector_1",
         cex.lab=0.8)
abline(v=0,col="red",lty=2)
dotchart(Sector_Soc,pch=20,labels=rownames(X), main= "PC3 : Sector_Soc",
         cex.lab=0.8)
abline(v=0,col="red",lty=2)

## Representación simultanea

windows(height=10,width=15)
fviz_pca_biplot(PCA, repel = TRUE,axes=c(1,2),
                col.var = "Blue", col.ind = "gray")
windows(height=10,width=15)
fviz_pca_biplot(PCA, repel = TRUE,axes=c(1,3),
                col.var = "Blue", col.ind = "gray")
windows(height=10,width=15)
fviz_pca_biplot(PCA, repel = TRUE,axes=c(2,3),
                col.var = "Blue", col.ind = "gray")

#------------------------------------------------------------------------------#
##     2.3 Proyección de Departamentos suplementarios                       ####
#------------------------------------------------------------------------------#

Factores= PCA_ind$coord[,1:3]
Proy.out= predict(PCA,newdata=PIB[index.out,])$coord[,1:3]
Factores= rbind(Factores,Proy.out)


windows(height=10,width=15)
par(mfrow=c(2,2))
plot(Factores[,1:2],pch=20,xlab="PC1 : Sector 23",ylab="PC2 : Sector 1")
grid()
abline(h=0,v=0,lty=2, col="red")
text(Factores[,1:2],rownames(Factores),cex=0.8,col="gray",pos=3)

plot(Factores[,2:3],pch=20,xlab="PC2 : Sector 1",ylab="PC3 : Sector Soc")
grid()
abline(h=0,v=0,lty=2, col="red")
text(Factores[,2:3],rownames(Factores),cex=0.8,col="gray",pos=3)

plot(Factores[,c(1,3)],pch=20,xlab="PC1 : Sector 23",ylab="PC3 : Sector Soc")
grid()
abline(h=0,v=0,lty=2, col="red")
text(Factores[,c(1,3)],rownames(Factores),cex=0.8,col="gray",pos=3)

#------------------------------------------------------------------------------#
##     3. Agrupación Departamentos - Conglomerados                          ####
#------------------------------------------------------------------------------#
# En este caso optaremos por agrupar las proyecciones en Factores, en lugar de agrupar PIB

K=5                      # Incialmente definiremos 5 grupos


set.seed(101)            # Semilla aleatoria
km_clusters <- kmeans(x = Factores, centers = K, nstart = 50,iter.max=1000)

km_clusters

Grupos=km_clusters$cluster

# Evaluación del Número adecuado de cluster

Evaluar_k=function(n_clust,data,iter.max,nstart){
  km <- kmeans(x = data, centers = n_clust, nstart = nstart,iter.max=iter.max)
  return(km$tot.withinss)
  }

k.opt=2:10
Eval_k=sapply(k.opt,Evaluar_k,data=Factores,iter.max=1000,nstart=50)

windows(height=10,width=15)
plot(k.opt,Eval_k,type="l",xlab="Número Cluster",ylab="SSE")

s = silhouette(Grupos, dist(Factores))     # Silhoutte plot
windows()
plot(s)

# Representación grafica de los cluster

windows(height=10,width=15)
fviz_cluster(object=km_clusters, data = Factores, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
             axes=c(1,2))
windows(height=10,width=15)
fviz_cluster(object=km_clusters, data = Factores, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
             axes=c(1,3))
windows(height=10,width=15)
fviz_cluster(object=km_clusters, data = Factores, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
             axes=c(2,3))

PIBFactores= cbind(PIB,Factores,Grupos)
col.cluster <- c("orange","purple","aquamarine1","red1",'yellow')[Grupos]
pairs(PIB)
pairs(PIB[,11:12],col=col.cluster,pch=19)
##################################Fin Tema 4 ###############################