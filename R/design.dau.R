`design.dau` <-
function (trt1, trt2, r,serie=2,seed=0,kinds="Super-Duper",name="trt")
{
number<-10
if(serie>0) number<-10^serie
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
nr<-as.numeric(table(block))
nt<-length(nr)
plots<-NULL
for(i in 1:nt) plots<-c(plots,i*number+1:nr[i])
book<-data.frame(block=as.factor(block),trt=as.factor(mtr))
book<-book[order(book[,1]),]
for (i in 1:r)
book[book[,1]==i,2]<-sample(book[book[,1]==i,2],length(book[book[,1]==i,2]))
book<-data.frame(plots,book)
rownames(book)=1:nrow(book)
names(book)[3]<-name
return(book) }

