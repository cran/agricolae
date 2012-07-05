`design.ab` <-
function(A,B,r,number=1,seed=0,kinds="Super-Duper"){
p<-length(A)
q<-length(B)
t<-rep(0,r*p*q)
dim(t)<-c(p*q,r)
ntr<-p*q
if(seed != 0) set.seed(seed,kinds)
trt<-random.ab(p,q)
bloque<-c(rep(1,ntr))
for (y in 2:r){
bloque<-c(bloque,rep(y,ntr))
trt<- rbind(trt, random.ab(p,q))
}
n<-nrow(trt)
factor1<-rep(NA,n)
factor2<-rep(NA,n)
for ( i in 1:n){
factor1[i]<-A[trt[i,1]]
factor2[i]<-B[trt[i,2]]
}
plots <- number + 1:(ntr * r) - 1
#libro<-cbind(parcela,bloque,trat)
book<-data.frame(plots,block=as.factor(bloque),factor1=as.factor(factor1),
factor2=as.factor(factor2))
names(book)[c(3,4)]<-c(paste(deparse(substitute(A))),paste(deparse(substitute(B))))
return(book) }

