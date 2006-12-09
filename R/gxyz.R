"gxyz" <-
function(zz) {
xx<-as.numeric(dimnames(zz)[[1]])
yy<-as.numeric(dimnames(zz)[[2]])
fila<-length(xx)
col<-length(yy)
total<-fila*col
x<-numeric(length=total)
y<-numeric(length=total)
z<-numeric(length=total)
k<-0
for(i in 1:fila){
for(j in 1:col) {
k<-k+1
x[k]<-xx[i]
y[k]<-yy[j]
z[k] <-zz[i,j]
}
}
zz<-data.frame(x,y,z)
return(zz)
}

