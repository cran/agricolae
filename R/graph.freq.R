"graph.freq" <-
function(breaks,counts, ...){
k<-length(counts)
mids<-rep(0,k)
ancho<-rep(0,k)
for (i in 1:k) {
mids[i]<-(breaks[i]+breaks[i+1])/2
ancho[i]<-(breaks[i+1]-breaks[i])
}
a<-breaks[1]-ancho[1]/2
b<-breaks[k+1]+ancho[k]/2
histograma<-list(breaks=breaks,counts=counts,mids=mids)
altura<-round(1.1*max(counts),0)
plot(c(a, b), c(0, altura), type= "n", xlab="",ylab="",axes=FALSE)
axis(1,breaks)
delta <- round(altura/6,0)
axis(2,seq(0,altura,delta))
for (j in 1:k) {
rect(breaks[j],0,breaks[j+1],counts[j], ...)
}
return(histograma)
}

