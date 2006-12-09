"bar.err" <-
function(x,std=TRUE,horiz=FALSE, ...) {
y<-x[,2]
names(y)<-x[,1]
if( std ) {
nivel0<-x[,2]-x[,5]*sqrt(x[,4])
nivel1<-x[,2]+x[,5]*sqrt(x[,4])
}
else {
nivel0<-x[,2]-x[,5]
nivel1<-x[,2]+x[,5]
}
n<-length(y)
indice<-barplot(y,horiz=horiz, ...)
tope<-max(nivel1)/20
for ( i in 1:n) {
if (horiz)  {
lines(rbind(c(nivel0[i],indice[i]),c(nivel1[i],indice[i])),col="red")
text( cex=1,nivel0[i],indice[i],"[")
text( cex=1,nivel1[i],indice[i],"]")
}
else {
lines(rbind(c(indice[i],nivel0[i]),c(indice[i],nivel1[i])),col="red")
text( cex=1,indice[i],nivel0[i],"---")
text( cex=1,indice[i],nivel1[i],"---")
}
}
}

