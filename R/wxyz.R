`wxyz` <-
function(model,x,y,z) {
datos<-data.frame(x,y,z)
lista<-by(datos,list(x=x,y=y),mean)
pp<-as.matrix(lista)
m<-nrow(pp)
n<-ncol(pp)
x1<-rownames(pp)
xx<-as.numeric(x1)
y1<-colnames(pp)
yy<-as.numeric(y1)
z1<-rep(NA,m*n)
dim(z1)<-c(m,n)
for (i in 1:m){
for (j in 1:n){
if( (length(pp[i,j][[1]][3]) > 0) &&  !(is.na(pp[i,j][[1]][3]) )) z1[i,j]<- pp[i,j][[1]][3] 
else z1[i,j]<-predict(model,data.frame(x=xx[i],y=yy[j]))
}
}
dimnames(z1)<-list(x1,y1)
return(z1)
}

