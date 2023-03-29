#------------------------------------------------------------------------------#
#                  Universidad del Valle - Escuela de Estadística              #
#                       Programa Académico de Estadística                      #
#        Asignatura : Técnicas de Minería de Datos y Aprendizaje Automático    #
#                      Estadístico - Jaime Mosquera Restrepo                   #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#          0. Configuración inicial-Librerias requeridas                    ####
#------------------------------------------------------------------------------#

wd="G:\\Mi unidad\\Docencia\\MDAA\\Script R"  # Establecer el dir de trabajo
setwd(wd)                                      

install.packages("easypackages")              # Libreria especial para hacer carga automática de librerias
library("easypackages")

lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","corrplot","editrules")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)         # Verificación, instalación y carga de librerias.

#------------------------------------------------------------------------------#
#                    1. Data Cleaning                                       ####
#                    Herramientas y Casos de estudio                           #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## 1.1. Caso 1, Visualización automática, Covid-19, Datos INS-Colombia      ####
#------------------------------------------------------------------------------#

##  Lectura automática de datos y ajuste de la estructura

url="https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD"
download.file(url,"COVID3.csv")                                                         # Descarga delos datos al directorio de trabajo

Covid=read.table("COVID.csv",header=T,sep=",",stringsAsFactors = F,encoding ="UTF-8",na.strings=c("N/A",""),quote="", fill=T)  # Lectura de Datos en R
View(Covid)
str(Covid)                                                 # Verificar la estructura de las variables

## Selección inicial de las variables, usaremos solo 10 variables de la hoja de datos   
#Covid=Covid[,c(2,19,6,7,5,16,8,10:13)] # ubicación de las alumnas en la hoja de datos 2022

Covid=Covid[,c(1,14,3:10)]              # ubicación de las columnas en la hoja de datos 2020
colnames(Covid)=c("id","fechadx","cod.munic","ciudad","departamento","condicion","edad","sexo",
                  "tipo","estado")                                       # Modificamos a nuestro gusto el nombre de variables
str(Covid)
summary(Covid)

table(Covid$condicion)
table(Covid$estado)
table(Covid$tipo)
table(Covid$sexo)


# Declaración de niveles correctos para las variables tipo Factor
level_condicion=c(Activo="Activo",Fallecido="Fallecido",fallecido="Fallecido",Recuperado="Recuperado")
level_estado=c(Fallecido="Fallecido",Grave="Grave",leve="Leve", Leve="Leve", LEVE="Leve",Moderado = "Moderado", moderado = "Moderado" )
level_tipo=c("En estudio"="En Estudio","En Estudio"="En Estudio",  "EN ESTUDIO"="En Estudio",  Enestudio="En Estudio", Importado="Importado", relacionado="Relacionado", Relacionado="Relacionado", RELACIONADO="Relacionado" )
level_sexo=c("m"="M","M"="M","f"="F", "F"="F")


## Modificación del formato y transformación de variables
Covid = transform(Covid,
                  sexo=factor(dplyr::recode(sexo,!!!level_sexo)),
                  condicion=factor(dplyr::recode(condicion, !!!level_condicion),levels=c("Recuperado","Casa","Hospital","Hospital UCI","Fallecido"),ordered=T),
                  estado=factor(dplyr::recode(estado, !!!level_estado), levels=c("Asintomático","Leve","Moderado","Grave","Fallecido"),ordered=T),
                  tipo=factor(dplyr::recode(tipo, !!!level_tipo)),
                  fechadx=lubridate::parse_date_time(fechadx,"Ymd HMS", truncated=3)                                   # Conversión a formato fecha
)
str(Covid)
summary(Covid)                                  


##  !Los datos están preparados¡ --> Visualización de Datos.

Covid = Covid%>%dplyr::arrange(fechadx)           # Ordenamiento de los datos - cronologico ascendente

  
  attach(Covid)                                   # Abre la Lectura de objetos (Variables) dentro de Covid, evita colocar el $ para las funciones
  Inicio = fechadx[1]                               # Fecha Primer caso.
  Dias.acum = difftime(na.omit(fechadx),Inicio,units= "days")  
  Dias.acum = unique(Dias.acum)
  Casos.dia = unname(table(fechadx))
  Casos.acum= cumsum(Casos.dia)
  
  
  # Visualización de Resultados.
  windows(height=10,width=20)                     # Abre una nueva ventana grafica con las  dimensiones predeterminadas
  M=matrix(c(1,1,2,2,1,1,2,2,3,5,6,6,4,5,6,6), nrow=4,byrow=F)
  layout(M)                                       # Partición de la ventana
  
  plot(Dias.acum,Casos.dia, type="h",col="Blue",xlim=c(0,max(Dias.acum)+3))
  lines(spline(Dias.acum,Casos.dia,n=100))
  plot(Dias.acum,Casos.acum,type="l", col="Blue",xlim=c(0,max(Dias.acum)+3))
  barplot(table(sexo),main="Casos por Genero",col=c("gray","blue"))
  barplot(table(condicion), main="Condición Actual",names=levels(condicion),cex.names=0.8)
  hist(edad, main="Distribución de la Edad en Covid", col="blue")
  boxplot(edad~condicion*sexo,names=rep(levels(condicion),2),ylim=c(0,110),cex.names=0.8,col=rep(c("gray","blue"),each=5))
  abline(v=5.5,lty=2,lwd=2)
  detach(Covid)                                   # Cierra la apertura de los objetos en COVID
  

# Verificado el funcionamiento del código, con ajustes, se convierte en función 

Visualizar.covid=function(city=NULL){

#url="https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD"
#download.file(url,"COVID2.csv")                                                         # Descarga delos datos al directorio de trabajo
#Covid2=read.table("COVID2.csv",header=T,sep=",",stringsAsFactors = F,encoding ="UTF-8",na.strings="N/A",quote="", fill=FALSE)   # Lectura de Datos en R
#Covid=Covid[,c(2,19,6,7,5,16,8,10:13)] # ubicación de las alumnas en la hoja de datos 2022
  
#Covid=Covid[,c(1,14,3:10)]              # ubicación de las columnas en la hoja de datos 2020
#colnames(Covid)=c("id","fechadx","cod.munic","ciudad","departamento","condicion","edad","sexo","tipo","estado") 
# Declaración de niveles correctos para las variables tipo Factor
# Declaración de niveles correctos para las variables tipo Factor
  #level_condicion=c(Activo="Activo",Fallecido="Fallecido",fallecido="Fallecido",Recuperado="Recuperado")
  #level_estado=c(Fallecido="Fallecido",Grave="Grave",leve="Leve", Leve="Leve", LEVE="Leve",Moderado = "Moderado", moderado = "Moderado" )
  #level_tipo=c("En estudio"="En Estudio","En Estudio"="En Estudio",  "EN ESTUDIO"="En Estudio",  Enestudio="En Estudio", Importado="Importado", relacionado="Relacionado", Relacionado="Relacionado", RELACIONADO="Relacionado" )
  #level_sexo=c("m"="M","M"="M","f"="F", "F"="F")
  
  
  ## Modificación del formato y transformación de variables
  #Covid = transform(Covid,
                    #sexo=factor(dplyr::recode(sexo,!!!level_sexo)),
                    #condicion=factor(dplyr::recode(condicion, !!!level_condicion),levels=c("Recuperado","Casa","Hospital","Hospital UCI","Fallecido"),ordered=T),
                    #estado=factor(dplyr::recode(estado, !!!level_estado), levels=c("Asintomático","Leve","Moderado","Grave","Fallecido"),ordered=T),
                    #tipo=factor(dplyr::recode(tipo, !!!level_tipo)),
                    #fechadx=lubridate::parse_date_time(fechadx,"Ymd HMS", truncated=3)                                   # Conversión a formato fecha
  #)
#Covid = Covid%>%dplyr::arrange(fechadx)           # Ordenamiento de los datos - cronologico ascendente
    

if(is.null(city)){city="Colombía"}                      # Ajuste 1. Incluido para filtrar la hoja de datos
else{Covid = Covid%>%filter(ciudad==city)} 
N=nrow(Covid)   ##Modificado##  

with(Covid,{                                   # Abre la Lectura de objetos (Variables) dentro de Covid, evita colocar el $ para las funciones
Inicio = fechadx[1]                               # Fecha Primer caso.
Dias.acum = difftime(na.omit(fechadx),Inicio,units= "days")  
Dias.acum = unique(Dias.acum)
Casos.dia = unname(table(fechadx))
Casos.acum= cumsum(Casos.dia)


# Visualización de Resultados.
windows(height=10,width=20)                     # Abre una nueva ventana grafica con las  dimensiones predeterminadas
M=matrix(c(1,1,2,2,1,1,2,2,3,5,6,6,4,5,6,6), nrow=4,byrow=F)
layout(M)                                       # Partición de la ventana

plot(Dias.acum,Casos.dia, type="h",col="Blue",xlim=c(0,max(Dias.acum)+3))
lines(spline(Dias.acum,Casos.dia,n=100))
mtext(paste(city," - ", N, "Contagios Covid - ", now()),3,line=2,cex=0.8) #Ajuste 3. Leyenda con fecha y ciudad de consulta
plot(Dias.acum,Casos.acum,type="l", col="Blue",xlim=c(0,max(Dias.acum)+3))
barplot(table(sexo),main="Casos por Genero",col=c("gray","blue"))
barplot(table(condicion), main="Condición Actual",names=levels(condicion),cex.names=0.8)
hist(edad, main="Distribución de la Edad en Covid", col="blue")
boxplot(edad~condicion*sexo,names=rep(levels(condicion),2),ylim=c(0,110),cex.names=0.8,col=rep(c("gray","blue"),each=5))
abline(v=5.5,lty=2,lwd=2)
})                                              #Cierra el with, equivalente al detach

}

# Ejemplo de Uso de la función para Colombia, Cali, Medellin, Bogotá

Visualizar.covid()
Visualizar.covid(city="Cali")
Visualizar.covid(city="Medellín")

#------------------------------------------------------------------------------#
#### 1.2. Validación de reglas de consistencia en los datos - editrules     ####
#------------------------------------------------------------------------------#
rm(list=ls())       # Antes de Seguir, vamos a eliminar todos los objetos del proyecto anterior

age=c(21,2,18,21,34)
agegroup=c("adult","child","adult","elderly","child")
height=c(6.0, 3.0 , 5.7, 5.0, -7.0)
status = c("single","married","married","widowed","married")
yearsmarried=c(-1,0,20,2,3)

Datos = data.frame(age,agegroup,height,status,yearsmarried)
rm(age,agegroup,height,status,yearsmarried)


# Carga del archivo de reglas de validación
Rules = editrules::editfile("Rules_File.txt")

# Conexión entre las  reglas
windows()
plot(Rules)

# Verificación de las reglas sobres los datos
editrules::violatedEdits(Rules, Datos)
Valid_Data = editrules::violatedEdits(Rules, Datos)
summary(Valid_Data)

# Visualización del diagnóstico
windows()
plot(Valid_Data)

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### 1.3. Identificación y cuantificación de datos faltantes                ####
#------------------------------------------------------------------------------#
rm(list=ls())       # Antes de Seguir, vamos a eliminar todos los objetos del proyecto anterior

# Ejemplo previo ilustrativo

Nombre <- c("Alejandro", "Katterine","Jaime","Herney","Kevin", "Gina")
Genero <- c("H", "M", "H","H","H","M")
Edad <- c(23,NA,40,NA,24, 28)
Equipo <- c("América", "Indiferente","América","Cali",NA,"Cali")

Datos <- data.frame(Genero,Edad,Equipo)
rownames(Datos)<-Nombre

rm(Nombre,Genero,Edad, Equipo)                  # Borrar los objetos innecesarios

View(Datos)
is.na(Datos)                                    # para cada elemento de Datos verifica si es NA
x11()
visdat::vis_miss(Datos)                          # Una función que visualiza los datos faltantes en la hoja de calculo

# Una función (Desarrollo propio: Evalua e identifica los datos faltantes por variable e individuo)
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
     barplot(Var_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por variable")
      axis(2,at=coord,labels=names(Var_per), cex.axis=0.5,pos=0,las=2)
      axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
   
      coord<-barplot(Obs_per,plot=F)
      barplot(Obs_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por registro")
      axis(2,at=coord,labels=names(Obs_per),cex.axis=0.5,pos=0,las=2)
      axis(1,seq(0,1,0.2),seq(0,1,0.2))
      }
   return(invisible(lista))
}

Summary.NA = miss(Datos)                        # Asignamos el resultado a un objeto lista para consultarlo
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### 1.4. Caso 2 (Airquality), Identificación y manejo de datos faltantes    ####
#------------------------------------------------------------------------------#

rm(Datos,Summary.NA)       # Antes de Seguir, vamos a eliminar todos los objetos del proyecto anterior

## Lectura de datos Airquality
Datos = data.frame(airquality)


str(Datos)

## Ajuste del formato de Variables y visualización rápida
Datos = transform(Datos,                                                                          
          Month=factor(Month,levels=5:9,labels=month.abb[5:9],ordered=T),                                    # Conversión a formato fecha
          Day=factor(Day,levels=1:31,labels=1:31))
str(Datos)
summary(Datos)

## Visualizando y Cuantificando los datos faltantes

is.na(Datos)                                 
windows(height=10,width=15); 
x11()
visdat::vis_miss(Datos) 
Summary.NA = miss(Datos,T)                     

## Lo que sucede cuando trabajamos con datos faltantes
Visualizar.AQ= function(Datos){   #Una función para visualizar los datos AQ
   with(Datos,{
   ## Tendencia Temporal
   windows(height=10,width=15)
   par(mfrow=c(2,2))
   plot(Ozone,type="l",col="Red")
   plot(Solar.R,type="l",col="Blue")
   plot(Wind,type="l",col="Gray")
   plot(Temp,type="l",col="Green")
   
   ## Variación mensual
   windows(height=10,width=15)
   par(mfrow=c(2,2))
   plot(Ozone~Month,type="l",col="Red")
   plot(Solar.R~Month,type="l",col="Blue")
   plot(Wind~Month,type="l",col="Gray")
   plot(Temp~Month,type="l",col="Green")
   })
   
   ## Correlación entre covariables cuantitativas
   Datos.cuant=Datos[,1:4]
   AQ.cor = cor(Datos.cuant,method="pearson")
   print(AQ.cor)
   windows(height=10,width=15)
   corrplot::corrplot(AQ.cor, method = "ellipse",addCoef.col = "black",type="upper")
   windows(height=10,width=15)
   pairs(Datos.cuant,lower.panel = panel.smooth, pch = 15)
   }

## Visualización.
Visualizar.AQ(Datos)


## Una primera solución, identificar y completar.

     ## Opción poco Probable

## Una segunda aproximación, omitir los registros con datos faltantes.

Datos_clean=na.omit(Datos)                    # se omiten todos los registros con datos faltantes
windows(height=10,width=15); visdat::vis_miss(Datos_clean) 
Visualizar.AQ(Datos_clean)

## Tercera aproximación, Imputar

# Imputación por la media.
mean(Datos$Ozone)
mean(Datos$Ozone,na.rm=T)
mean(Datos$Solar.R,na.rm=T)

imputM = mice::mice(Datos, maxit = 1, method = "mean",seed = 2018,print=F)
Datos_ImputM = mice::complete(imputM)
windows(height=10,width=15); visdat::vis_miss(Datos_ImputM) 
Visualizar.AQ(Datos_ImputM)

## Imputación por regresion.
imputR = mice::mice(Datos, maxit = 1, method = "norm.predict",seed = 2018,print=F)
Datos_ImputR = mice::complete(imputR)
windows(height=10,width=15); visdat::vis_miss(Datos_ImputR) 
Visualizar.AQ(Datos_ImputR)
x11()
boxplot(Datos_ImputR$Ozone); abline(h=0,col="red")

## Imputación por vecindad (KNN)
Datos_ImputKNN<-DMwR2::knnImputation(Datos,k=5,scale=T,meth = "weighAvg")
windows(height=10,width=15); visdat::vis_miss(Datos_ImputKNN)
Visualizar.AQ(Datos_ImputKNN)
x11()
boxplot(Datos_ImputKNN$Ozone); abline(h=0,col="red")

### La imputación puede cambiar los resultados de los modelos - Una comparación

# Un modelo de regresión

model_miss=lm(Ozone~.,Datos[,-6])             # Con los Datos Faltantes
summary(model_miss)

model_clean=lm(Ozone~.,Datos_clean[,-6])      # Eliminando registros con Datos Faltantes
summary(model_clean)

model_ImputM=lm(Ozone~.,Datos_ImputM[,-6])    # Con Imputación por la media
summary(model_ImputM)

model_ImputR=lm(Ozone~.,Datos_ImputR[,-6])    # Con Imputación por Regresión
summary(model_ImputR)

model_ImputKNN=lm(Ozone~.,Datos_ImputKNN[,-6])        # Con Imputación por Vecindad
summary(model_ImputKNN)
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### 1.5 Caso 3, Identificación y manejo de datos atípicos e influyentes     ####
#------------------------------------------------------------------------------#

## Datos modelación del Ozono
url <- "https://raw.githubusercontent.com/selva86/datasets/master/ozone.csv" 
#download.file(url,"ozone.csv")
#Datos <- read.csv("ozone.csv")
Datos <- read.csv(url)

# Datos Faltantes
x11(); visdat::vis_miss(Datos) 

# Transformación de variables 
str(Datos)
Datos = transform(Datos,                                                                          
          Month=factor(Month,levels=1:12,labels=month.abb,ordered=T),                                    # Conversión a formato fecha
          Day_of_week =factor(Day_of_week,levels=1:5,labels=c("Lunes","Martes","Miercoles","Jueves","Viernes"),ordered=T))
str(Datos)
summary(Datos)

# Identificación y visualización de outliers Univariados.
# Iniciaremos con la primera Variable: ozone_reading

x11()
par(mfrow=c(3,1))
with(Datos,{
   hist(ozone_reading,freq=F,col="blue",breaks=13)
   boxplot(ozone_reading,horizontal=T,col="blue")
   hist(scale(ozone_reading),freq=F,col="blue",breaks=13)
}
)

##Criterio Estandarización
id = which(abs(scale(Datos$ozone_reading))>3)
Datos$ozone_reading[id]
Datos[id,]

##Criterio Boxplot
boxplot.stats(Datos$ozone_reading)$out
which(Datos$ozone_reading%in%boxplot.stats(Datos$ozone_reading)$out)

##Criterio Distancia Cooks
mean(Datos$ozone_reading)
model=lm(ozone_reading~1,data=Datos);CD=cooks.distance(model)
id_Ozono=unname(which(CD>4*mean(CD)))
windows()
labels=1:nrow(Datos);labels[-id_Ozono]="."
plot(CD,pch=20);abline(h=4*mean(CD),col="red",ylab="Cooks_Distance")
text(id_Ozono,CD[id_Ozono],id_Ozono, col="red",pos=3,cex=0.8)

### Este procedimiento tendriamos que repetirlo para todas las variables, un poco demandante, mejor lo ponemos en una función.

id.out.uni=function(x,method=c("Standarized","Tukey","Cook")){
   id.out=NULL
   if(method=="Standarized"){id.out=which(abs(scale(x))>3)}
   else if(method=="Tukey"){id.out=which(x%in%(boxplot.stats(x)$out))}
   else if(method=="Cook"){model=lm(x~1);CD=cooks.distance(model)
      id.out=unname(which(CD>4*mean(CD)))}
   return(id.out)
}

# Miremos como funciona la función
id.out.uni(Datos$ozone_reading,method="Standarized")
id.out.uni(Datos$ozone_reading,method="Tukey")
id.out.uni(Datos$ozone_reading,method="Cook")

### Ahora vamos a automatizar la inspección de las variables

#Visualizar
windows()
par(mfrow=c(2,5))
lapply(Datos[,-(1:3)],boxplot,col="Blue")

### Identificar los Datos Atipicos
out_Stand = lapply(Datos[,-(1:3)],id.out.uni,method="Standarized")
out_Tukey = lapply(Datos[,-(1:3)],id.out.uni,method="Tukey")
out_Cook = lapply(Datos[,-(1:3)],id.out.uni,method="Cook")

####### Identificación  multivariada de outliers
Ozone.cor = cor(Datos[,-(1:3)],method="pearson")
windows(height=10,width=15)
corrplot::corrplot(Ozone.cor , method = "ellipse",addCoef.col = "black",type="upper")
windows(height=10,width=15)
pairs(Datos[,-(1:3)],lower.panel = panel.smooth, pch = 15)

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

id_Out_mult=out.mult(Datos[,-(1:3)])


##################################Fin Capitulo 2 ###############################









