X<- Datos_con_NA
X<- na.omit(X)
View(X)
library(openxlsx)
write.xlsx(X,"SinNA.xlsx")
pairs(X[,-1])
model<- lm(Suicidio~Desempleo+Alcohol+Discapacidad,data=X)
summary(model)
