"coeff.diana" <-
function(data) {
metodo<-c("euclidean","maximum","manhattan","canberra","binary","minkowski")
nm<-length(metodo)
sdata<-data
nc<-ncol(data)
nr <-nrow(data)
# 
coeff<-rep(0,nm)
for (i in 1:nm){
distancia<-dist(data,method=metodo[i])
coeff[i]<-diana(distancia)$dc
}
names(coeff)<-metodo
coeff<-na.omit(coeff)
maximo<-max(coeff,na.rm=TRUE)
r<-length(coeff)
for (i in 1:r) {
if (coeff[i] == maximo) metodo1<- names(coeff)[i]
}
cat("\nSelection of the best method \n")
cat("Maximum coefficient of divisibilidad:\n\n")
cat(maximo,"\n\n")
cat("Better method for calculation of distance:\n\n")
cat(metodo1,"\n\n")
#---------------------
distancia<-dist(data,method=metodo1)
return(distancia)
}

