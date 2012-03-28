`design.lsd` <-
function (trt,number=1,seed=0,kinds="Super-Duper",first=FALSE)
{
r <- length(trt)
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
m<-sample(2:r,r-1)
a<-a[,c(1,m)]
if (first) {
	m<-sample(1:r,r)
	a<-a[m,]
}
trat<-trt[a]
columna <- rep(gl(r, 1), r)
fila <- gl(r, r)
plots <- number + 1:length(trat) - 1
book <- data.frame(plots, row = as.factor(fila), col = as.factor(columna),
		trat = as.factor(trat))
names(book)[4] <- c(paste(deparse(substitute(trt))))
return(book)
}

