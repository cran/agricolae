`design.rcbd` <-
function (trt, r,number=1,seed=0,kinds="Super-Duper",first=FALSE )
{
ntr <- length(trt)
if(seed != 0) set.seed(seed,kinds)
mtr <- sample(trt, ntr, replace = FALSE)
block <- c(rep(1, ntr))
for (y in 2:r) {
block <- c(block, rep(y, ntr))
mtr <- c(mtr, sample(trt, ntr, replace = FALSE))
}
if(!first) mtr[1:ntr]<-trt
if (!first)
	mtr[1:ntr] <- trt
plots <- number + 1:(ntr * r) - 1
book <- data.frame(plots, block = as.factor(block), trt = as.factor(mtr))
names(book)[3] <- c(paste(deparse(substitute(trt))))
return(book)
}



