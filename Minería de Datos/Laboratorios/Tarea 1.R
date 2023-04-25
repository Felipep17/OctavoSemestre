#------------------------------------------------------------------------------#
#                  Universidad del Valle - Escuela de Estadística              #
#                       Programa Académico de Estadística                      #
#        Asignatura : Técnicas de Minería de Datos y Aprendizaje Automático    #
#                      Estadísticos - Andrés Felipe Palomino Ronaldo Hernández  #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#          0. Configuración inicial-Librerias requeridas                    ####
#------------------------------------------------------------------------------#
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Minería de Datos/Laboratorios")
# Establecer el dir de trabajo
# Libreria especial para hacer carga automática de librerias
library("easypackages")

lib_req<-c("psych","lubridate","dplyr","visdat","missMDA","mice","DMwR2","corrplot","editrules")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)         # Verificación, instalación y carga de librerias.

#------------------------------------------------------------------------------#
#                    1. Data Cleaning                                       ####
#                    Herramientas y Casos de estudio                           #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
library(readxl)
calcium<-  read_excel("calcium.xls",col_types = c("numeric",
                                                  "numeric", "text", "numeric", "text",
                                                  "numeric", "numeric", "text"),na = c("N/A","","NA"))
calcium<- as.data.frame(calcium)
str(calcium)                                                 # Verificar la estructura de las variables
View(calcium)
#
calcium<- calcium[,-1]
str(calcium)
table(calcium$Sex)
table(calcium$Lab)
table(calcium$AgeG)
#Identificación de indices de los valores con problemáticas en escritura
calcium[which(calcium$Sex %in% c(12, 21, 22,"F","f","M","m")),]
calcium[which(calcium$Lab%in% c(21 ,43)),]
calcium[which(calcium$AgeG%in% '85-89'),]
#
level_sex<-c('1'='1','2'='2','f'='2','M'='1','F'='2','22'='2','m'='1','M'='1','21'='1','12'='2')
level_AgeG<-c("65 - 69"="65 - 69", "70 - 74"="70 - 74" ,"75 - 79"="75 - 79" ,"80 - 84"="80 - 84","85-89"= "85 - 89" , "85 - 89"="85 - 89")
level_lab<- c('1'='1','2'='2','3'='3','4'='4','5'='5','6'='6','43'='4','21'='2')
#Se revisa el data.frame y se corrige
# Se hace la adecuación de 
calcium = transform(calcium,
                  Sex=factor(dplyr::recode(Sex,!!!level_sex)),
                  AgeG=factor(dplyr::recode(AgeG, !!!level_AgeG),levels=c("65 - 69","70 - 74","75 - 79","80 - 84","85 - 89"),ordered=T),
                  Lab=factor(dplyr::recode(Lab, !!!level_lab), levels=c("1","2","3","4","5",'6'))
                  )                                   

str(calcium)
summary(calcium)  
# Datos faltantes
# Una función (Desarrollo propio: Evalua e identifica los datos faltantes por variable e individuo)
visdat::vis_miss(calcium)             
miss<-function(Datos,plot=T){  
  n=nrow(Datos);p=ncol(Datos)
  names.obs<-rownames(Datos)
  
  nobs.comp=sum(complete.cases(Datos))         # Cuenta los registros completos
  Obs.comp=which(complete.cases(Datos))        # Identifica los registros completos
  nobs.miss = sum(!complete.cases(Datos))      # Identifica los registros con datos faltantes.
  Obs.miss=which(!complete.cases(Datos))       # Identifica los registros con datos faltantes.
  
  Datos.NA<-is.na(Datos)
  Var_Num<- sort(colSums(Datos.NA),decreasing=T)
  Var_per<-round(Var_Num/n,3)
  Obs_Num<-rowSums(Datos.NA)
  names(Obs_Num)<-names.obs
  Obs_Num<-sort(Obs_Num,decreasing=T)
  Obs_per<-round(Obs_Num/p,3)
  lista<-list(n.row = n, n.col = p,n.comp = nobs.comp,Obs.comp = Obs.comp,n.miss = nobs.miss,Obs.miss = Obs.miss, Var.n = Var_Num , Var.p = Var_per, Obs.n= Obs_Num, Obs.per= Obs_per)
  
  if(plot){
    windows(height=10,width=15)
    par(mfrow=c(1,2))
    coord<-barplot(Var_per,plot=F)
    barplot(Var_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% Datos faltantes por variable",panel.first=grid(),las=2,col=c('aquamarine3'))
    axis(2,at=coord,labels=names(Var_per), cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
    
    coord<-barplot(Obs_per,plot=F)
    barplot(Obs_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% Datos faltantes por registro",panel.first=grid(),las=2,col=c('aquamarine3'))
    axis(2,at=coord,labels=names(Obs_per),cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2))
  }
  return(invisible(lista))
}
resumen.na = miss(calcium) 
calcium[resumen.na$Obs.miss,]
######################  
calcium$Age[c(4,14,105)]<- c(73,76,NA)
calcium$ALP[22]<- 64
calcium$CaMol[85]<- NA
calcium$AgeG[79]<-"80 - 84"
summary(calcium)
#
resumen.na = miss(calcium) 
resumen.na$n.miss
# Visualización de verdaderos datos faltantes
# Declaración de niveles correctos para las variables tipo Factor


#Identificación de datos atípicos
# Identificación y visualización de outliers Univariados.
par(mfrow=c(1,2))
ind<- c(1,3,5,6) #Variables con tipos de variables cuantitativas
boxplot(calcium[,ind[1:2]],pch=19,col=c('aquamarine4','red4'))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium[,ind[1:2]],pch=19,col=c('aquamarine4','red4'),add=T)
boxplot(calcium[,ind[3:4]],pch=19,col=c('aquamarine4','red4'))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium[,ind[3:4]],pch=19,col=c('aquamarine4','red4'),add=T)
#
id.out.uni=function(x,method=c("Standarized","Tukey","Cook")){
  id.out=NULL
  if(method=="Standarized"){id.out=which(abs(scale(x))>3)}
  else if(method=="Tukey"){id.out=which(x%in%(boxplot.stats(x)$out))}
  else if(method=="Cook"){model=lm(x~1);CD=cooks.distance(model)
  id.out=unname(which(CD>4*mean(CD)))}
  return(id.out)
}
### Ahora vamos a automatizar la inspección de las variables
### Identificar los Datos Atipicos
out_Stand = lapply(calcium[,ind],id.out.uni,method="Standarized") # Se escoge este método dado la enorme dimensión de las escalas de las variables
out_Tukey = lapply(calcium[,ind],id.out.uni,method="Tukey")
out_Cook = lapply(calcium[,ind],id.out.uni,method="Cook")
calcium[out_Stand[[1]],]
calcium$Age[out_Stand[[1]]]<- c(71,69,73)
calcium[out_Stand[[2]],]
calcium$ALP[out_Stand[[2]]]<-c(213,193,219)
calcium[out_Stand[[3]],]
calcium$CaMol[c(out_Stand[[3]],26)]<- c(2.53,2,2.23,2.43,2.5,2.33,2.4,2.5,2.5,2.35,2.25,2.45,2.33)
calcium[out_Stand[[4]],]
calcium$PhoMol[out_Stand[[4]]]<- c(1.23,0.84)
#Nueva visualización
out_Stand = lapply(calcium[,ind],id.out.uni,method="Standarized") # Se escoge este método dado la enorme dimensión de las escalas de las variables
out_Tukey = lapply(calcium[,ind],id.out.uni,method="Tukey")
out_Cook = lapply(calcium[,ind],id.out.uni,method="Cook")
# Identificación y visualización de outliers Univariados.
par(mfrow=c(1,2))
ind<- c(1,3,5,6) #Variables con tipos de variables cuantitativas
boxplot(calcium[,ind[1:2]],pch=19,col=c('aquamarine4','red4'))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium[,ind[1:2]],pch=19,col=c('aquamarine4','red4'),add=T)
boxplot(calcium[,ind[3:4]],pch=19,col=c('aquamarine4','red4'))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium[,ind[3:4]],pch=19,col=c('aquamarine4','red4'),add=T)
#Nueva identificación
####### Identificación  multivariada de outliers
pairs(calcium[,ind],lower.panel = panel.smooth, pch = 15)

## Visualización de outliers multivariados
out.mult=function(Datos){
  n= nrow(Datos); p= ncol(Datos)
  Distance= mahalanobis(Datos,center=colMeans(Datos),cov=cov(Datos))
  Limit= qchisq(0.01, lower.tail=F,df=p)
  id.dist= which(Distance>Limit)
  Score_LOF = DMwR2::lofactor(Datos, k=5)
  id.LOF <- order(Score_LOF, decreasing=T)[1:ceiling(0.01*n)]
  
  windows()
  par(mfrow=c(2,1))
  plot(Distance,pch=20,ylim=c(0,max(Distance)*1.2))
  text(id.dist,Distance[id.dist],id.dist, col="red",pos=3,cex=0.8)
  abline(h=Limit,col="red",lwd=2,lty=2)
  plot(Score_LOF,pch=20,ylim=c(0,max(Score_LOF)*1.2))
  text(id.LOF,Score_LOF[id.LOF],id.LOF, col="red",pos=3,cex=0.8)
  return(list(Out_dist=id.dist,Out_LOF=id.LOF))
}

id_Out_mult=out.mult(na.omit(calcium[,ind]))
calcium[id_Out_mult$Out_LOF,]
calcium[id_Out_mult$Out_dist,]
## Imputación por vecindad (KNN)
Datos_ImputKNN<-DMwR2::knnImputation(calcium,k=5,scale=T,meth = "weighAvg")
windows(height=10,width=15); visdat::vis_miss(Datos_ImputKNN)
par(mfrow=c(1,2))
boxplot(Datos_ImputKNN[,ind[1:2]],pch=19,col=c('aquamarine4','red4'))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Datos_ImputKNN[,ind[1:2]],pch=19,col=c('aquamarine4','red4'),add=T)
boxplot(Datos_ImputKNN[,ind[3:4]],pch=19,col=c('aquamarine4','red4'))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Datos_ImputKNN[,ind[3:4]],pch=19,col=c('aquamarine4','red4'),add=T)
#------------------------------------------------------------------------------#
#Reglas de validación
#Carga del archivo de reglas de validación
Rules = editrules::editfile("Validacion.txt")

# Conexión entre las  reglas
plot(Rules)

# Verificación de las reglas sobres los datos
editrules::violatedEdits(Rules, Datos_ImputKNN)
Valid_Data = editrules::violatedEdits(Rules, Datos_ImputKNN)
summary(Valid_Data)

# Visualización del diagnóstico
windows()
plot(Valid_Data)
#Visualización
# Visualización de Resultados
barplot(table(Datos_ImputKNN$Sex),col=c('aquamarine3','pink'),main='Distribución por sexo',panel.first=grid())
barplot(table(Datos_ImputKNN$Lab),col=c('aquamarine1','aquamarine2','aquamarine3','aquamarine4','blue','blue4'),main='Distribución por laboratorio',panel.first=grid())
barplot(table(Datos_ImputKNN$AgeG),col=c('aquamarine1','aquamarine2','aquamarine3','aquamarine4','blue','blue4'),main='Distribución por G.Edad',panel.first=grid(),las=2)
# ALP vs Sex Lab
boxplot(Datos_ImputKNN$ALP~Datos_ImputKNN$Sex*Datos_ImputKNN$Lab,names=rep(levels(Datos_ImputKNN$Lab),2),cex.names=0.8,col=rep(c("pink","blue"),each=6),las=2,ylab='ALP',main='ALP by Sex and Lab',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Datos_ImputKNN$ALP~Datos_ImputKNN$Sex*Datos_ImputKNN$Lab,pch=19,col=rep(c("pink","blue"),each=6),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=6.5,lwd=2,lty=2)
# ALP vs Sex Age G
boxplot(Datos_ImputKNN$ALP~Datos_ImputKNN$Sex*Datos_ImputKNN$AgeG,names=rep(levels(Datos_ImputKNN$AgeG),2),cex.names=0.8,col=rep(c("pink","blue"),each=5),las=2,ylab='ALP',main='ALP by Sex and AgeG',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Datos_ImputKNN$ALP~Datos_ImputKNN$Sex*Datos_ImputKNN$AgeG,pch=19,col=rep(c("pink","blue"),each=5),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=5.5,lwd=2,lty=2)
# CaMol vs Sex Lab
boxplot(Datos_ImputKNN$CaMol~Datos_ImputKNN$Sex*Datos_ImputKNN$Lab,names=rep(levels(Datos_ImputKNN$Lab),2),cex.names=0.8,col=rep(c("pink","blue"),each=6),las=2,ylab='ALP',main='CaMol by Sex and Lab',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Datos_ImputKNN$CaMol~Datos_ImputKNN$Sex*Datos_ImputKNN$Lab,pch=19,col=rep(c("pink","blue"),each=6),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=6.5,lwd=2,lty=2)
# CaMol vs Sex Age G
boxplot(calcium$CaMol~calcium$Sex*calcium$AgeG,panel.first=grid(),names=rep(levels(calcium$AgeG),2),cex.names=0.8,col=rep(c("pink","blue"),each=5),las=2,ylab='ALP',main='CaMol by Sex and AgeG',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium$CaMol~calcium$Sex*calcium$AgeG,pch=19,col=rep(c("pink","blue"),each=5),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=5.5,lwd=2,lty=2)
# PhoMol vs Sex Lab
boxplot(calcium$PhoMol~calcium$Sex*calcium$Lab,names=rep(levels(calcium$Lab),2),cex.names=0.8,col=rep(c("pink","blue"),each=6),las=2,ylab='ALP',main='PhoMol by Sex and Lab',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium$PhoMol~calcium$Sex*calcium$Lab,pch=19,col=rep(c("pink","blue"),each=6),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=6.5,lwd=2,lty=2)
# PhoMol vs Sex Age G
boxplot(calcium$PhoMol~calcium$Sex*calcium$AgeG,panel.first=grid(),names=rep(levels(calcium$AgeG),2),cex.names=0.8,col=rep(c("pink","blue"),each=5),las=2,ylab='ALP',main='PhoMol by Sex and AgeG',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium$PhoMol~calcium$Sex*calcium$AgeG,pch=19,col=rep(c("pink","blue"),each=5),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=5.5,lwd=2,lty=2)
psych::pairs.panels(Datos_ImputKNN[,ind], 
                    method = "pearson", # correlation method
                    hist.col = "aquamarine1",
                    density = TRUE,  # show density plots
                    ellipses = TRUE # show correlation ellipses
)
