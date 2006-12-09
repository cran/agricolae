"grid3d" <-
function(zz,m,n,...) {
xx<-as.numeric(dimnames(zz)[[1]])
yy<-as.numeric(dimnames(zz)[[2]])
xmax<-max(xx); xmin<-min(xx); ymax<-max(yy); ymin<-min(yy)
x<-seq(xmin,xmax,length=m)
y<-seq(ymin,ymax,length=n)
z<-numeric(length=m*n)
dim(z)<-c(m,n)
for (k in 1:m) {
for (v in 1:n) {
z[k,v]<-interpolate(c(x[k],y[v]),zz,...)
}
}
dimnames(z)<-list(x,y)
return(z)
}

