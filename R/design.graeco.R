`design.graeco` <-
function (trt1, trt2, number = 1, seed = 0, kinds = "Super-Duper")
{
r <- length(trt1)

if (floor(r/2)*2 == r ) {
if (r == 6 | r ==10 | r > 13) {
cat("not implemented design ",r,"x",r,", see help(design.graeco)\n")
return()
}}
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

if ((r == 4) | (r==8) | (r==12)) {
#Case 4
if (r == 4) {
c1 <-c(1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1)
c2 <-c(1,2,3,4,4,3,2,1,2,1,4,3,3,4,1,2)
}
if (r == 8) {
c1 <-c(1:8,5,8,4,3,1,7,6,2,2,1,7,6,8,4,3,5,3,7,1,5,4,8,2,6,7,3,2,8,6,5,1,4,
4,6,5,1,3,2,8,7,8,5,6,7,2,3,4,1,6,4,8,2,7,1,5,3)
c2 <-c(1:8,2,1,7,6,8,4,3,5,3,7,1,5,4,8,2,6,4,6,5,1,3,2,8,7,5,8,4,3,1,7,6,2,
6,4,8,2,7,1,5,3,7,3,2,8,6,5,1,4,8,5,6,7,2,3,4,1)
}
if (r == 12) {
c1 <-c(1:12,12:9,4:1,8:5,6,5,8,7,10,9,12,11,2,1,4,3,7,8,5,6,11,12,9,10,3,4,1,2,
5:12,1:4,4:1,8:5,12:9,10,9,12,11,2,1,4,3,6,5,8,7,11,12,9,10,3,4,1,2,7,8,5,6,
9:12,1:8,8:5,12:9,4:1,2,1,4,3,6,5,8,7,10,9,12,11,3,4,1,2,7,8,5,6,11,12,9,10)
c2 <-c(1:12,2,1,4,3,6,5,8,7,10,9,12,11,3,4,1,2,7,8,5,6,11,12,9,10,4:1,8:5,12:9,
9:12,1:4,5:8,10,9,12,11,2,1,4,3,6,5,8,7,11,12,9,10,3,4,1,2,7,8,5,6,12:9,4:1,8:5,
5:12,1:4,6,5,8,7,10,9,12,11,2,1,4,3,7,8,5,6,11,12,9,10,3,4,1,2,8:5,12:9,4:1)
}
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
