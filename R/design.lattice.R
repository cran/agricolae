`design.lattice` <-
function(trt,r=3,serie=2,seed=0,kinds="Super-Duper") {
number<-10
if(serie>0) number<-10^serie
ntr<-length(trt)
k<-sqrt(ntr)
if(r==2) type="simple"
if(r==3) type="triple"
if(seed != 0) set.seed(seed,kinds)
cat("\nLattice design, ", type, " ",k,"x",k,"\n")
E1 <- (ntr - 1) * (2 - 1)/((ntr - 1) * (2 - 1) + 2 * (k-1))
E2 <- (ntr - 1) * (3 - 1)/((ntr - 1) * (3 - 1) + 3 * (k-1))
parameters<-data.frame(treatmens=ntr,blockSize=k,blocks=k)
rownames(parameters)<-"values"
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
trt1<-c(as.numeric(t(c1)),as.numeric(t(c2)),as.numeric(t(c3)))
Rep<-as.numeric(sqr)
plots <- Rep*number+(1:ntr)
plan<-data.frame(plots,r=factor(Rep),block=factor(block),trt=factor(trt[trt1]))
C1<-trt[c1] ; dim(C1)<-dim(c1)
C2<-trt[c2] ; dim(C2)<-dim(c2)
C3<-trt[c3] ; dim(C3)<-dim(c3)
if (type=="triple") {
cat("\nEfficiency design ", E2,"\n")
parameters<-data.frame(parameters,r=3,Efficiency=E2)
return(list(parameters=parameters,square1=C1,square2=C2,square3=C3,plan=plan))
}
if (type=="simple") {
parameters<-data.frame(parameters,r=2,Efficiency=E1)
cat("\nEfficiency design ", E1,"\n")
return(list(parameters=parameters,square1=C1,square2=C2,plan=subset(plan,as.numeric(plan[,2])<3)))
}
}
