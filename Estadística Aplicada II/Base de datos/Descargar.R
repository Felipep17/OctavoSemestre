X<-as.data.frame(yarn$NIR)
y<- as.data.frame(yarn$density)
z<- as.data.frame(yarn$train)
library(openxlsx)
write.xlsx(X,'NIR.xlsx')
write.xlsx(y,'density.xlsx')
write.xlsx(z,'train.xlsx')
yarn$density
View(yarn)
X<- yarn
ncol(yarn)
plot(yarn[,1:10])
plot(density~NIR.50,data=X,pch=19,panel.first=grid())
x<- X[,50]
summary(x)
resumen<- function(x){
  X<- matrix(0,9,1)
  resumen<- round(c(mean(x),median(x),min(x),max(x),var(x),sd(x),quantile(x,0.25),quantile(x,0.75),sd(x)/mean(x)),4)
  for( i  in 1:7){
    X[i,]<- resumen[i]
  }
  rownames(X)<-c('Media','Mediana','Min','Max','Var','Sd','1st Qu.','3rd Qu','Coef.Var')
  colnames(X)<- ('Estadística Descriptivas')
  return(X)
}
resumen(x)
library(xtable)
xtable(resumen(x))
