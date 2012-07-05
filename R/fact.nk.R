`fact.nk` <-
function(level,factors,blocks,seed=0,kinds="Super-Duper")
{
if(seed != 0) set.seed(seed,kinds)
t<-level^factors
a<-runif(t)
for(k in 1:factors){
j<- level^(factors-k)
x<-rep(0,j)
for (ii in 1:(level-1)) x<-c(x,rep(ii,j))
l<-k-1
if (l>0) {
for(i in 1:l) {
x<-rep(x,level)
}
}
a<-rbind(a,x)
}
nombres<-LETTERS[1:factors]
dimnames(a)<-list(c("blocks",nombres),1:t)
combinado<-t(a)
factorial<-combinado[order(combinado[,1]),]
factorial[,1]<-1
if (blocks > 1) {
for (i in 2:blocks){
combinado[,1]<-runif(t)
combinado<-combinado[order(combinado[,1]),]
combinado[,1]<-i
factorial<-rbind(factorial,combinado)
}
}
row.names(factorial)<-seq(1,nrow(factorial))
plots<-1:(t*blocks)
return(data.frame(plots=plots,factorial))
}

