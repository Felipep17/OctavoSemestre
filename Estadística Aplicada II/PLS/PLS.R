fPLS1na <- function(YX){ 
  Z <- as.matrix(YX)
  Xo <- scale(YX[,-1])	# matriz n.p
  p <- ncol(Xo); n <- nrow(Xo)
  Yo <- scale(YX[,1]); yz <- Yo
  contary<- sum(is.na(Yo))/length(Yo) #Contar Na's y
  ind<- which(is.na(Yo))
  contarcolumna<-c();  contarfila<- c()
  for(i in 1:dim(Xo)[2]){
    if(sum(is.na(Xo[,i]))/dim(Xo)[1]>=0.3){
      contarcolumna[i]<- 1
    }
    else{
      contarcolumna[i]<-0
    }
  }
  for(i in 1:dim(Xo)[1]){
    if(sum(is.na(Xo[i,]))/dim(Xo)[2]>=0.3){
      contarfila[i]<- 1
    }
    else{
      contarfila[i]<-0
    }
  }
  contarfila<- sum(contarfila) #Contador NA's filas
  contarcolumna<- sum(contarcolumna)#Contador NA's filas
  if(contarfila <1 & contarcolumna<1 & contary==0){
  if(any(!is.finite(Z)))H <- p	
  else H <- qr(Xo)$rank
  cv2 <- matrix(0,1,H)		
  
  W <- matrix(0,p,H); W. <- matrix(0,p,H)
  T <- matrix(0,n,H); C <- matrix(0,1,H)
  P <- matrix(0,p,H); B <- matrix(0,p,H)
  
  for(h in 1:H)
  {
    for(j in 1:p)
    {
      wh. <- na.omit(cbind(Xo[,j],Yo))
      W.[j,h] <- sum(wh.[,1]*wh.[,2])/sum(wh.[,2]^2)
    }
    
    nW. <- sqrt(sum(W.[,h]^2))
    W[,h] <- W.[,h]/nW.
    
    for(i in 1:n)
    {
      ti <- na.omit(cbind(Xo[i,],W[,h]))
      T[i,h] <- sum(ti[,1]*ti[,2])/sum(ti[,2]^2)
    }
    
    th <- T[,h] ; cv2[1,h] <- cov(th,yz)^2 
    
    ch. <- na.omit(cbind(th,Yo))
    ch <- sum(ch.[,1]*ch.[,2])/sum(ch.[,1]^2)
    C[,h] <- ch
    
    for(j in 1:p)
    {
      ph. <- na.omit(cbind(Xo[,j],th))
      P[j,h] <- sum(ph.[,1]*ph.[,2])/sum(ph.[,2]^2)
    }
    ph <- P[,h]
    
    X1 <- Xo - th%*%t(ph); Xo <- X1
    Y1 <- Yo - th%*%t(ch); Yo <- Y1
    
    B[,h] <- W[,1:h]%*%(solve(t(P[,1:h])%*%W[,1:h]))%*%C[1:h]
    
  } # end h
  RAdj<- c()
  diff<- c()
  for(i in 1:ncol(T)){
    model<- lm(scale(yz)~T[,1:ncol(T)])
    resumen <- summary(model)
    RAdj[i] <- resumen$adj.r.squared
    
  }
  for(i in 2:ncol(T)){
    diff[i]<- RAdj[i]-RAdj[i-1]
    
  }
  ncomp<- (which(diff < 0.05))
  r.PLS1na <- list(W,T,C,P,B,cv2,ncomp)
  
  return(r.PLS1na)
}
if(contarfila <1 & contarcolumna<1 & contary> 0 & contary<0.3){
      if(any(!is.finite(Z)))H <- p	
      else H <- qr(Xo)$rank
      cv2 <- matrix(0,1,H)		
      
      W <- matrix(0,p,H); W. <- matrix(0,p,H)
      T <- matrix(0,n,H); C <- matrix(0,1,H)
      P <- matrix(0,p,H); B <- matrix(0,p,H)
      
      for(h in 1:H)
      {
        for(j in 1:p)
        {
          wh. <- na.omit(cbind(Xo[,j],Yo))
          W.[j,h] <- sum(wh.[,1]*wh.[,2])/sum(wh.[,2]^2)
        }
        
        nW. <- sqrt(sum(W.[,h]^2))
        W[,h] <- W.[,h]/nW.
        
        for(i in 1:n)
        {
          ti <- na.omit(cbind(Xo[i,],W[,h]))
          T[i,h] <- sum(ti[,1]*ti[,2])/sum(ti[,2]^2)
        }
        
        th <- T[,h] ; cv2[1,h] <- cov(th,yz)^2 
        
        ch. <- na.omit(cbind(th,Yo))
        ch <- sum(ch.[,1]*ch.[,2])/sum(ch.[,1]^2)
        C[,h] <- ch
        
        for(j in 1:p)
        {
          ph. <- na.omit(cbind(Xo[,j],th))
          P[j,h] <- sum(ph.[,1]*ph.[,2])/sum(ph.[,2]^2)
        }
        ph <- P[,h]
        
        X1 <- Xo - th%*%t(ph); Xo <- X1
        Y1 <- Yo - th%*%t(ch); Yo <- Y1
        
        B[,h] <- W[,1:h]%*%(solve(t(P[,1:h])%*%W[,1:h]))%*%C[1:h]
        
      } # end h
      RAdj<- c()
      diff<- c()
      for(i in 1:ncol(T)){
        model<- lm(scale(yz)~T[,1:ncol(T)])
        resumen <- summary(model)
        RAdj[i] <- resumen$adj.r.squared
        
      }
      for(i in 2:ncol(T)){
        diff[i]<- RAdj[i]-RAdj[i-1]
        
      }
      ncomp<- (which(diff < 0.05))
      r.PLS1na <- list(W,T,C,P,B,cv2,ncomp)
      
      return(r.PLS1na)
      
    }
  else{
    print("Warning Demasiados NA'S por fila o columna o en Y")
  }
} # end fPLS1na con y sin datos completos   

setwd("C:/Users/sebas/OneDrive/Escritorio/PLS")
library(car)

YX <- read.table("Cornell.txt",header=TRUE)
rowSums(YX[,-1])	# 1 : multicolineal

reg <- lm(y~.,data=YX)	# descarta la variable x7 
fpls1. <- fPLS1na(YX)
ncomp<-fpls1.[[7]][1]
w <- fpls1.[[1]]
T <- fpls1.[[2]]
c <- fpls1.[[3]]	# y^ = t.c
P <- fpls1.[[4]]
b <- fpls1.[[5]]# y^ = c1t1+ c2t2+ c3t3 = X.b
y <- scale(YX[,1])
rgYT.1 <- lm(y~T[,1]-1)	
summary(rgYT.1)
# y^ = 0.482t1 , R2=0.9236
# y^ = Xb = -0.210x1+..-0.186x7

rgYT.2 <- lm(y~T[,1:2]-1)
summary(rgYT.2)	# y^ = 0.482t1+0.273t2 , R2=0.9763
# y^ = Xb[,2] = -0.178x1+..-0.127x7

# ..... Aplicacion Cornell  NAs ....
  
  fmd <- function(Xo,a)	# Genera a: % NAs , md: miss data
  {
    X. <- as.matrix(Xo)
    n <- nrow(X.); p <- ncol(X.); N <- n*p
    m <- sample(N,round(a*N,0))
    
    X.[m] <- NA
    
    return(X.)
  }

Xi <- YX[,-1]; Yi <- YX[,1]

Xna <- fmd(Xi,0.1)
# 15% d'NA aleatorios
YXna <- cbind(Yi,Xna) ; YXna

fpls1 <- fPLS1na(YXna)

T <- fpls1[[2]]	# cor(T) : no ortog
labs <- paste(c("T"),1:7,sep=""); colnames(T) <- labs

w <- fpls1[[1]]	# cor(w) : no ortog
ncomp<-fpls1[[7]][1]
c <- fpls1[[3]]	# y^ = c.t
P <- fpls1[[4]]
b <- fpls1[[5]]	# y^ = X.b
cor(T) #No ortogonales
mean(T[,1])
var(T[,1])
fpls1 <- fPLS1na(YXna)

T <- fpls1[[2]]	# cor(T) : no ortog
labs <- paste(c("T"),1:7,sep=""); colnames(T) <- labs
w <- fpls1[[1]]	# cor(w) : no ortog
c <- fpls1[[3]]	# y^ = c.t
P <- fpls1[[4]]
b <- fpls1[[5]]	# y^ = X.b
model<- lm(scale(Yi)~T[,1:ncomp]-1)
summary(model)
#####################################
Xi <- YX[,-1]; Yi <- YX[,1]
Yi<- fmd(Yi,0.05)
Xna <- fmd(Xi,0.1)
YXna <- cbind(Yi,Xna) ; YXna
YXna <- cbind(Yi,Xna) ; YXna
fpls1 <- fPLS1na(YXna)
T <- fpls1[[2]]	# cor(T) : no ortog
labs <- paste(c("T"),1:7,sep=""); colnames(T) <- labs
w <- fpls1[[1]]	# cor(w) : no ortog
c <- fpls1[[3]]	# y^ = c.t
P <- fpls1[[4]]
b <- fpls1[[5]]	# y^ = X.b
ncomp<- fpls1[[7]][1]
##############################
#imputación de y
yimput<- matrix(0,length(Yi),ncomp)
for( i in 1:ncomp){
  yimput[,i]<- as.matrix(t(c[i]%*%T[,i]))
}
yimput<- rowSums(yimput)
Yi<-scale(Yi)
Yi[which(is.na(Yi))]<-yimput[which(is.na(Yi))]
Yinew<-scale(Yi)
######## Nuevamente PLS
Xi <- YX[,-1]; Yi <- Yinew
Xna <- fmd(Xi,0.1)
YXna <- cbind(Yi,Xna) ; YXna
YXna <- cbind(Yi,Xna) ; YXna
fpls1 <- fPLS1na(YXna)
T <- fpls1[[2]]	# cor(T) : no ortog
labs <- paste(c("T"),1:7,sep=""); colnames(T) <- labs
w <- fpls1[[1]]	# cor(w) : no ortog
c <- fpls1[[3]]	# y^ = c.t
P <- fpls1[[4]]
b <- fpls1[[5]]	# y^ = X.b
ncomp<- fpls1[[7]][1]
summary(lm(Yi~T[,1:ncomp]))
