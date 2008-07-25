`design.lattice` <-
function(k,type="triple",number=1,seed=0,kinds="Super-Duper") {
if(seed != 0) set.seed(seed,kinds)
cat("\nLattice design, ", type, " ",k,"x",k,"\n")
seed=1
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
sqr<-gl(3,k*k)
nb <- as.numeric(gl(k,k))
block<-c(nb,nb+k,nb+2*k)
# tercer cuadro
latino<-as.character(design.lsd(1:k)[,4])
Z<-as.numeric(t(c1))
c3<-Z[order(latino)]
dim(c3)<-c(k,k)
c3<-t(c3)
s<-sample(1:k,k,replace=FALSE)
c1<-c1[s,]
s<-sample(1:k,k,replace=FALSE)
c2<-c2[s,]
s<-sample(1:k,k,replace=FALSE)
c3<-c3[s,]
trt<-c(as.numeric(t(c1)),as.numeric(t(c2)),as.numeric(t(c3)))
plots<-(number-1)+1:(3*k*k)
plan<-data.frame(plots,sqr=sqr,block=block,trt=trt)
if (type=="triple") return(list(square1=c1,square2=c2,square3=c3,plan=plan))
if (type=="simple") return(list(square1=c1,square2=c2,plan=subset(plan,plan[,3]<3)))
}
