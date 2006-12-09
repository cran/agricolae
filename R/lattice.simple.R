"lattice.simple" <-
function(k,number=1,seed=0,kinds="Super-Duper") {
if(seed != 0) set.seed(seed,kinds)
cat("\nLattice design, ",k,"x",k,"\n")
c1<-rep(0,k*k)
dim(c1)<-c(k,k)
c2<-c1
for (a in 1:k) {
for (b in 1:k) {
p<-k*(a-1)+b # primer  cuadro
c1[a,b]<-p
}
}
c2<-t(c1)
# randomiza cada cuadro
nt<-k*k
t<-1:nt
s<-sample(t,nt)
for (a in 1:k) {
for (b in 1:k) {
c1[a,b]<-s[c1[a,b]]
}
}
    c2 <- t(c1)
    nt <- k * k
    t <- 1:nt
sqr<-gl(2,k*k)
block<-c(as.numeric(gl(k,k)),as.numeric(gl(k,k))+k)
a1<-c(as.numeric(t(c1)),as.numeric(t(c2)))
plots<-(number-1)+1:(2*k*k)
a1<-data.frame(plots,sqr=sqr,block=block,trt=a1)
return(list(square1=c1,square2=c2,plan=a1))
}

