"intervals.freq" <-
function(breaks){
n<-length(breaks)-1
y<-rep(0,2*n)
dim(y)<-c(n,2)
for (i in 1:n) {
y[i,1]<-breaks[i]
y[i,2]<-breaks[i+1]
}
colnames(y)<-c("inf","sup")
return(y)
}

