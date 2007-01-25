"grid3p" <-
function(x,y,z,m,n) {
xmax<-max(x); xmin<-min(x); ymax<-max(y); ymin<-min(y)
x2<-seq(xmin,xmax,length=m)
y2<-seq(ymin,ymax,length=n)
z2<-numeric(length=m*n)
dim(z2)<-c(m,n)
for (k in 1:m) {
for (v in 1:n) {
z2[k,v]<-interpp(x,y,z,x2[k],y2[v])$z
}
}
dimnames(z2)<-list(x2,y2)
return(z2)
}

