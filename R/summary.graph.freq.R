`summary.graph.freq` <-
function(object,...){
xx<-object$mids
yy<-object$counts
y1<-sum(yy)
zz<-object$breaks
x<-length(xx)
acum<-0
z<-rep(0,7*x)
dim(z)<-c(x,7)
for (i in 1:x) {
z[i,1]<-zz[i]
z[i,2]<-zz[i+1]
z[i,3]<-xx[i]
z[i,4]<-yy[i]
z[i,5]<-yy[i]/y1
z[i,6]<-yy[i]+acum
acum<-z[i,6]
z[i,7]<-z[i,6]/y1
}
colnames(z)<-c("Inf","Sup","MC","fi","fri","Fi","Fri")
rownames(z)<-rep(" ",x)
return(z)
}

