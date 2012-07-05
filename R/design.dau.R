`design.dau` <-
function (trt1, trt2, r,number=1,seed=0,kinds="Super-Duper",name="trt")
{
ntr1 <- length(trt1)
if(seed != 0) set.seed(seed,kinds)
mtr1 <- sample(trt1, ntr1, replace = FALSE)
block <- c(rep(1, ntr1))
for (y in 2:r) {
block <- c(block, rep(y, ntr1))
mtr1 <- c(mtr1, sample(trt1, ntr1, replace = FALSE))
}
ntr2 <- length(trt2)
mtr2 <- sample(trt2,ntr2, replace = FALSE)
s<-s<-1:ntr2%%r
for(i in 1:ntr2) if(s[i]==0)s[i]<-r
block <- c(block, s)
mtr <- c(mtr1,mtr2)
plots <- number+1:(ntr1*r + ntr2)-1

book<-data.frame(block=as.factor(block),trt=as.factor(mtr))
book<-book[order(book[,1]),]
for (i in 1:r)
book[book[,1]==i,2]<-sample(book[book[,1]==i,2],length(book[book[,1]==i,2]))
book<-data.frame(plots,book)
rownames(book)=1:nrow(book)
names(book)[3]<-name
return(book) }

