#Codigo de tarea 1 Procesamiento de datos
#Tarea 1 curso mineria de datos

#Mesa

wd=setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Minería de Datos/Laboratorios")
setwd(wd)



##  1. Lea la hoja de datos y adecúe el formato de cada variable, verificando que dispone de una hoja de datos técnicamente correcta.####




#lectura de datos

#install.packages("easypackages")              # Libreria especial para hacer carga automática de librerias
library("easypackages")


lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","corrplot","editrules","readxl")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)         # Verificación, instalación y carga de librerias.

### lectura de la base de datos con ajustes para cada una de las variables.

calcium<-  read_excel("calcium.xls",col_types = c("numeric",
                                                  "numeric", "text", "numeric", "text",
                                                  "numeric", "numeric", "text"),na = c("N/A","","NA"))

#elimina la primera columna
calcium <- read_excel("calcium.xls",
                      col_types = c("skip", "numeric", "text", "numeric", "text",
                                    "numeric", "numeric", "text"),na = c("N/A", "", "NA"))


calcium$Sex<-as.factor(calcium$Sex)
calcium$Lab<-as.factor(calcium$Lab)
calcium$AgeG<-as.factor(calcium$AgeG)

View(calcium)
calcium
str(calcium)



#------------------------------------------------------------------------------------------#

##  2. Construya el archivo: consistencia.txt, en el cual incluya las ecuaciones que usted  ####
#considera necesarias para verificar la consistencia de los datos en el conjunto de
#variables. Aplique estas reglas sobre la hoja de datos y genere un pequeño reporte de
#sus resultados.


# Carga del archivo de reglas de validación
Rules = editrules::editfile("Reglas_Lab.txt")


# Conexión entre las  reglas
windows()
plot(Rules)

# Verificación de las reglas sobres los datos
Valid_Data = editrules::violatedEdits(Rules, calcium)
Valid_Data
summary(Valid_Data)

# Visualización del diagnóstico
windows()
plot(Valid_Data)


Valid_Data2<- data.frame(Valid_Data)

#verificacion de inconsitencia para la regla numero 4 que indica ALP <= 115
listado <- data.frame(calcium$ALP,Valid_Data2$num4)
listado

listado2 <- data.frame(calcium$CaMol,Valid_Data2$num5)
filter(listado2,listado2$Valid_Data2.num5 == TRUE)

listado3 <- data.frame(calcium$CaMol,Valid_Data2$num6)
filter(listado3,listado3$Valid_Data2.num6 == TRUE)


listado4 <- data.frame(calcium$PhoMol,Valid_Data2$num8)
filter(listado4,listado4$Valid_Data2.num8 == TRUE)

listado5 <- data.frame(calcium,Valid_Data2$dat12)
filter(listado5,listado5$Valid_Data2.dat12 == TRUE)

#juntar datos para observar  todas las inconsitencias
Valid_Data2=data.frame(editrules::violatedEdits(Rules, calcium))

reglasposicion=merge(cbind(colnames(Valid_Data2), col=1:length(Valid_Data2)),
                     which(Valid_Data2==T,arr.ind =T));b
reglasposicion


# Resumen de la verificacion de las reglas por variables cuantitativas:

# Edad: Se presentaron 3 individuos que presentan una edad mayor a 100 años por lo que
# parace error de codificaicon dado que se presentan valores extremos en 730 años,
# no se presentaron personas con edad menor a 65 años que es requerida para el estudio
#
# CaMol Calcium:  2 individuos presentan una medicion del calcio menor a los estipulado
# en el estudio de 2.1 y 15 individuos que tiene Calcium mayor a 2.7
#
# PhoMol Phosphorus: 7 individuos presentan una medicion de Phosphorus menor a la estipulada
# de 0.84 y  7 individuos tiene una concnetracion de Phosphorus mayor a 1.5
#
# ALP Fosfatasa Alcalina: 30 individuos presentan una concentracion mayor a la fijada por
# los expertos de 115 y 1 individuo que no cumple con el minimo de Fosfatasa Alcalina de 30
#
# Sex : 8 individuos tienen incorrecta la codificicacion de su sexo
#
# Lab : 3 individuos tienen incorrecta la codificacion del tipo de laboratorio
#
# AgeG : 1 individuo que tienen error en la forma del escritura del la categoria de los años

#---------------------------------------------------------------------------------------------#



##  3. Visualice e identifique los registros que presentan datos faltantes. ####
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
} ##funcion para ver posicion de los faltantes
resumen.na = miss(calcium)        #aplicacion de la funcin miss
resumen.na$n.miss



#identificacion de datos faltnates sin corregir los cuantitativos
calcium[resumen.na$Obs.miss,]     #posicion de los datos faltantes


#--------------------------------------------------------------------------------------------#

##  4. Con los resultados de los puntos 2 y 3, usted dispone de un listado con los registros inconsistentes y con datos faltantes. Es necesario corregirlo ####

summary(calcium$Age) # inconsistencia de edad de 771

#tabla de datos para Sex
table(calcium$Sex)
#Extrae el valor de la observacion osea la fila para scada una de las varaibles en la condicion
#indicada en caracter
which(calcium$Sex=='12'|calcium$Sex=='21'|calcium$Sex=='22')

#Pide el dataframe con la observacion creada por which como data frame para SEX
calcium[which(calcium$Sex %in% c(12, 21, 22,"F","f","M","m")),3]


#tabla de datos para Lab
table(calcium$Lab)
#Extrae el valor de la observacion osea la fila para scada una de las varaibles en la condicion

#Pide el dataframe con la observacion creada por which como data frame para Lab
calcium[which(calcium$Lab%in% c(21 ,43)),]


#tabla de datos para Lab
table(calcium$AgeG)
#Extrae el valor de la observacion osea la fila para scada una de las varaibles en la condicion

#Pide el dataframe con la observacion creada por which como data frame para Lab
calcium[which(calcium$AgeG%in% '85-89'),]




# Declaración de niveles correctos para las variables tipo Factor

level_Sex<-c("1"="1","2"="2","f"="2","F"="2","m"="1","M"="1","22"="2","21"="1","12"="2")
level_AgeG<-c("65 - 69"="65 - 69", "70 - 74"="70 - 74" ,"75 - 79"="75 - 79" ,"80 - 84"="80 - 84","85-89"= "85 - 89" , "85 - 89"="85 - 89")
level_Lab<-c("1"="1" , "2"="2" , "21"="2" ,"3"="3" , "4"="4" , "43"="4" ,"5"="5" , "6"="6")


#Revision de variables tipo Factor
require(dplyr)
calcium<-transform(calcium, AgeG=factor(dplyr::recode(AgeG,!!!level_AgeG)),
                   Sex=factor(dplyr::recode(Sex,!!!level_Sex)),
                   Lab=factor(dplyr::recode(Lab,!!!level_Lab)))
str(calcium)
summary(calcium)
levels(calcium$Lab)
levels(calcium$AgeG)

#visualizacion de los valores valtantes para variables numericas y corregir segun el Excel dado

#tabla de datos para Sex
table(calcium$Sex)
#Extrae el valor de la observacion osea la fila para scada una de las varaibles en la condicion
#indicada en caracter
which(calcium$Sex=='12'|calcium$Sex=='21'|calcium$Sex=='22')

#Pide el dataframe con la observacion creada por which como data frame para SEX
calcium[which(calcium$Sex %in% c(12, 21, 22,"F","f","M","m")),c(1,3)]


#tabla de datos para Lab
table(calcium$Lab)
#Extrae el valor de la observacion osea la fila para scada una de las varaibles en la condicion

#Pide el dataframe con la observacion creada por which como data frame para Lab
calcium[which(calcium$Lab%in% c(21 ,43)),]


#tabla de datos para Lab
table(calcium$AgeG)
#Extrae el valor de la observacion osea la fila para scada una de las varaibles en la condicion

#Pide el dataframe con la observacion creada por which como data frame para Lab
calcium[which(calcium$AgeG%in% '85-89'),]




# Declaración de niveles correctos para las variables tipo Factor

level_Sex<-c("1"="1","2"="2","f"="2","F"="2","m"="1","M"="1","22"="2","21"="1","12"="2")
level_AgeG<-c("65 - 69"="65 - 69", "70 - 74"="70 - 74" ,"75 - 79"="75 - 79" ,"80 - 84"="80 - 84","85-89"= "85 - 89" , "85 - 89"="85 - 89")
level_Lab<-c("1"="1" , "2"="2" , "21"="2" ,"3"="3" , "4"="4" , "43"="4" ,"5"="5" , "6"="6")


#Transformacion de variables tipo Factor
require(dplyr)
calcium<-transform(calcium, AgeG=factor(dplyr::recode(AgeG,!!!level_AgeG)),
                   Sex=factor(dplyr::recode(Sex,!!!level_Sex)),
                   Lab=factor(dplyr::recode(Lab,!!!level_Lab)))
str(calcium)
summary(calcium)
levels(calcium$Lab)
levels(calcium$AgeG)


which(calcium$Age %in% NA)


# corrige los valores cuantitativos
#Para Age
which(calcium$Age %in% NA) # muestra el numero de la fila
calcium$Age[c(4,14,105)]<- c(73,76,NA) #corrige segun la el excel suministrado
#Para ALP
which(calcium$ALP %in% NA)
calcium$ALP[22]<- 64
#Para CaMol
which(calcium$CaMol %in% NA)
calcium$CaMol[85]<- NA
#Para AgeG
which(calcium$AgeG %in% NA)
calcium$AgeG[79]<-"80 - 84"

summary(calcium)

#visualizacion de los faltantes finales despues de correcion de valores cuantitativos
vis_miss(calcium)
resumen.na = miss(calcium) #aplicacion de la funcin miss
resumen.na$n.miss

#-------------------------------------------------------------------------------------------------#

##  5. Sobre el conjunto de variables cuantitativas, realice un diagnóstico de datos atípicos.####
#    Utilice los dos enfoques, univariado y multivariado. Para cada dato atípico identificado,
#    decida si debe ser retenido o aislado del análisis de datos.




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
ind<- c(1,3,5,6) #Variables con tipos de variables cuantitativas
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




##  6. Ahora usted tiene una hoja de datos con unos pocos datos faltantes. Algunos de ellos####
#son originalmente ausentes y otros se convirtieron en ausentes por ser datos atípicos
#aislables. Sugiera el método adecuado para realizar la imputación de estos datos y
#ejecútela.


#visualizacion de los faltantes finales despues de correcion de valores cuantitativos
resumen.na = miss(calcium) #aplicacion de la funcin miss
resumen.na$n.miss           #numero de datos faltnates
calcium[resumen.na$Obs.miss,]

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
#Datos antes de imputar por KNN
calcium[resumen.na$Obs.miss,]
#Mismos datos despues de imputar por KNN
Datos_ImputKNN$Age<- as.integer(Datos_ImputKNN$Age)
Datos_ImputKNN[c(42,85,105,170),]


# Verificación de las reglas sobres los datos
editrules::violatedEdits(Rules, Datos_ImputKNN)
Valid_Data2 = editrules::violatedEdits(Rules, Datos_ImputKNN)
summary(Valid_Data2)

# Visualización del diagnóstico
windows()
plot(Valid_Data)

# 7. ReporteCambios.txt



#Visualización
# Visualización de Resultados
M=matrix(c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4,5,5,6,6), nrow=4,byrow=F)
layout(M)   
# CaMol vs Sex Lab
boxplot(Datos_ImputKNN$CaMol~Datos_ImputKNN$Sex*Datos_ImputKNN$Lab,names=rep(levels(Datos_ImputKNN$Lab),2),cex.names=0.8,col=rep(c("pink","blue"),each=6),las=2,ylab='CaMol',main='CaMol by Sex and Lab',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Datos_ImputKNN$CaMol~Datos_ImputKNN$Sex*Datos_ImputKNN$Lab,pch=19,col=rep(c("pink","blue"),each=6),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=6.5,lwd=2,lty=2)
#CaMol Sex and Age
boxplot(Datos_ImputKNN$CaMol~Datos_ImputKNN$Sex*Datos_ImputKNN$AgeG,names=rep(levels(Datos_ImputKNN$AgeG),2),cex.names=0.8,col=rep(c("pink","blue"),each=5),las=2,ylab='CaMol',main='CaMol by Sex and AgeG',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Datos_ImputKNN$CaMol~Datos_ImputKNN$Sex*Datos_ImputKNN$AgeG,pch=19,col=rep(c("pink","blue"),each=5),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=5.5,lwd=2,lty=2)
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
#
barplot(table(Datos_ImputKNN$Sex),col=c('aquamarine3','pink'),main='Distribución por sexo',panel.first=grid())
barplot(table(Datos_ImputKNN$Lab),col=c('aquamarine1','aquamarine2','aquamarine3','aquamarine4','blue','blue4'),main='Distribución por laboratorio',panel.first=grid())
par(mfrow=c(2,2))
barplot(table(Datos_ImputKNN$AgeG),col=c('aquamarine1','aquamarine2','aquamarine3','aquamarine4','blue','blue4'),main='Distribución por G.Edad',panel.first=grid(),las=2)
# CaMol vs Sex Age G
boxplot(calcium$CaMol~calcium$Sex*calcium$AgeG,panel.first=grid(),names=rep(levels(calcium$AgeG),2),cex.names=0.8,col=rep(c("pink","blue"),each=5),las=2,ylab='CaMol',main='CaMol by Sex and AgeG',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium$CaMol~calcium$Sex*calcium$AgeG,pch=19,col=rep(c("pink","blue"),each=5),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=5.5,lwd=2,lty=2)
# PhoMol vs Sex Lab
boxplot(calcium$PhoMol~calcium$Sex*calcium$Lab,names=rep(levels(calcium$Lab),2),cex.names=0.8,col=rep(c("pink","blue"),each=6),las=2,ylab='Pho Mol',main='PhoMol by Sex and Lab',xlab='')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(calcium$PhoMol~calcium$Sex*calcium$Lab,pch=19,col=rep(c("pink","blue"),each=6),add=T,axes=F,xlab='',ylab='',las=2)
abline(v=6.5,lwd=2,lty=2)
# PhoMol vs Sex Age G
boxplot(calcium$PhoMol~calcium$Sex*calcium$AgeG,panel.first=grid(),names=rep(levels(calcium$AgeG),2),cex.names=0.8,col=rep(c("pink","blue"),each=5),las=2,ylab='Pho Mol',main='PhoMol by Sex and AgeG',xlab='')
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
write.csv(Datos_ImputKNN, "clean_calcium.csv")
