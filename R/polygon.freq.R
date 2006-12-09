"polygon.freq" <-
function(histogram, ...){
xx<-histogram$mids
yy<-histogram$counts
zz<-histogram$breaks
x1 <- xx[1]-zz[2]+zz[1]
z<-length(zz)
x2<-xx[z-1]+zz[z]-zz[z-1]
xx<-c(x1,xx,x2)
yy<-c(0,yy,0)
lines(xx,yy, ...)
abline(h=0)
}

