x<- c(20,21.8,19.5)
y<- c(17,16,18)
z<- c(18,21,20)
w<- c(23,25,24)
a<- (sum(x+y))^2/6
b<- (sum(z+w))^2/6
t<- sum(x+y+z+w)^2/12
SC1<-a+b-t
c<- (sum(x+z))^2/6
d<- (sum(y+w))^2/6
SC2<-c+d-t
Total<-var(c(x,y,z,w))*(length(c(x,y,z,w)-1))
SCta<- sum(x)^2/3+sum(y)^2/3+sum(z)^2/3+sum(w)^2/3-a-b-c-d+t
Error<- Total-SCta-SC1-SC2
cm1<-SC1/(1)
cm2<- SC2/1
cmscta<- SCta
cmerror<- Error/8
##############
F1<-cm1/cmerror
F2<-cm2/cmerror
F3<-cmscta/cmerror
Anova<- matrix(c(1,SC1,SC1,F1,1,SC2,SC2,F2,1,SCta,SCta,F3,8,Error,cmerror,0),ncol=4,nrow=4,byrow=T)
colnames(Anova)<- c("g.l","SC","CM","F")
rownames(Anova)<-c("tau","alpha","interacción","Error")
Fs<- c(pf(F1,1,8,lower.tail = F),pf(F2,1,8,lower.tail = F),pf(F3,1,8,lower.tail = F),"")
Anova<- as.data.frame(Anova)
Anova$pval<- Fs
View(Anova)
porcentaje<- c(SC1/Total,SC2/Total,SCta/Total,(Total-SC1-SC2-SCta)/Total)
por<-as.data.frame(porcentaje)
rownames(por)<- rownames(Anova)
barplot(porcentaje,col=c("aquamarine1","aquamarine2","aquamarine3","aquamarine4"),horiz = T)
