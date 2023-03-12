library(pls)
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
plot(density~NIR.50,data=X)
