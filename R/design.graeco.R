`design.graeco` <-
function (trt1, trt2, number = 1, seed = 0, kinds = "Super-Duper")
{
r <- length(trt1)
plots<-number+1:(r^2)-1
col<- rep(gl(r,1),r)
row <- gl(r,r)
C1<-data.frame(plots,row,col)
C2<-C1
if(seed != 0) set.seed(seed,kinds)
a <- 1:(r * r)
dim(a) <- c(r, r)
for (i in 1:r) {
        for (j in 1:r) {
            k <- i + j - 1
            if (k > r)
                k <- i + j - r - 1
            a[i, j] <- k
        }
}
m <- sample(trt1, r)
C1<-data.frame(C1,m[a])
m <- sample(trt2, r)
C2<-data.frame(C2,m[a])
ntr<-length(trt1)

if (r == 4) {
#Case 4
c1 <-c(1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1)
c2 <-c(1,2,3,4,4,3,2,1,2,1,4,3,3,4,1,2)
t1<-sample(trt1,r,replace=FALSE)
t2<-sample(trt2,r,replace=FALSE)
t1<-t1[c1]
t2<-t2[c2]
C1 <-data.frame(C1[,1:3],t1)
C2 <-data.frame(C2[,1:3],t2)
}
C1<-data.frame(C1,B=0)
for(k in 1:r) {
x<-C1[k,4]
i<-1
for(j in 1:(r^2)) {
y<-C2[(k-1)*r+i,4]
if (C1[j,4]==x) {
C1[j,5]<-y
i<-i+1
}
}
}
C1[,4] <- as.factor(C1[,4])
names(C1)[4] <- c(paste(deparse(substitute(trt1))))
names(C1)[5] <- c(paste(deparse(substitute(trt2))))
C1[,5] <- trt2[C1[,5]]
C1[,5] <- as.factor(C1[,5])
return(C1)
}

