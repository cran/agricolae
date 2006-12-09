`design.graeco` <-
function (trt1, trt2, number = 1, seed = 0, kinds = "Super-Duper")
{
if(seed != 0) set.seed(seed,kinds)
C1 <-design.lsd(trt1)
C2 <-design.lsd(trt2)
ntr<-length(trt1)
if (ntr == 4) {
#Case 4
t1<-sample(trt1,4,replace=FALSE)
t2<-sample(trt2,4,replace=FALSE)
c1 <-c(1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1)
c2 <-c(1,2,3,4,4,3,2,1,2,1,4,3,3,4,1,2)
t1<-t1[c1]
t2<-t2[c2]
C1 <-data.frame(C1[,1:3],t1)
C2 <-data.frame(C2[,1:3],t2)
}
C1<-data.frame(C1,B=0)
for(k in 1:ntr) {
x<-C1[k,4]
i<-1
for(j in 1:(ntr^2)) {
y<-C2[(k-1)*ntr+i,4]
if (C1[j,4]==x) {
C1[j,5]<-y
i<-i+1
}
}
}
C1[,1] <- number+1:(ntr^2)-1
C1[,2] <- as.factor(C1[,2])
C1[,3] <- as.factor(C1[,3])
C1[,4] <- as.factor(C1[,4])
names(C1)[4] <- c(paste(deparse(substitute(trt1))))
names(C1)[5] <- c(paste(deparse(substitute(trt2))))
C1[,5] <- trt2[C1[,5]]
return(C1)
}

