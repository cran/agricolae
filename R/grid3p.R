"grid3p" <-
function(xx,yy,zz,m,n) {
xmax<-max(xx); xmin<-min(xx); ymax<-max(yy); ymin<-min(yy)
x<-seq(xmin,xmax,length=m)
y<-seq(ymin,ymax,length=n)
z2<-numeric(length=m*n)
dim(z2)<-c(m,n)
for (k in 1:m) {
for (v in 1:n) {
z2[k,v]<-interpp(xx,yy,zz,x[k],y[v])$z
}
}
dimnames(z2)<-list(x,y)
return(z2)
}

