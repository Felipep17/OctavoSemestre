#------------------------------------------------------------------------------#
#                  Universidad del Valle - Escuela de Estadística              #
#                            Programa Académico de:                            #
#             Esp. en Estadística Aplicada, MgSc Análitica e Int Negocios      #
#             Asignatura : Minería de Datos y Aprendizaje Automático           #
#                      Estadístico - Jaime Mosquera Restrepo                   #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
####         Funciones propias  - librería local - source                   ####
#------------------------------------------------------------------------------#

# Identificación de datos atipicos univariantes
id.out.uni=function(x,method=c("Standarized","Tukey","Cook")){
  id.out=NULL
  if(method=="Standarized"){id.out=which(abs(scale(x))>3)}
  else if(method=="Tukey"){id.out=which(x%in%(boxplot.stats(x)$out))}
  else if(method=="Cook"){model=lm(x~1);CD=cooks.distance(model)
  id.out=unname(which(CD>4*mean(CD)))}
  return(id.out)
}

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