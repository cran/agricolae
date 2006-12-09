"plot.disc" <-
function(x,y,disc,limx=c(min(x),max(x)),limy=c(min(y),max(y)),...) {
#
lc<-length(disc)
A<-data.frame(x,y)
f1<-subset(A, (A[,1] < disc[2]))
plot(f1,xlim=limx,ylim=limy,frame=FALSE,...)
for (j in 2:(lc-1)) {
fs<-subset(A, ((A[,1] > disc[j]) & (A[,1]< disc[j+1]) ) )
lines(fs,...)
}
grid(col="brown")
}

