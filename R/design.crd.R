`design.crd` <-
function(trt,r,number=1,seed=0,kinds="Super-Duper")
{
junto<-data.frame(trt,r)
junto<-junto[order(junto[,1]),]
TR<-as.character(junto[,1])
r<-as.numeric(junto[,2])
y <- rep(TR[1], r[1])
tr <- length(TR)
if(seed != 0) set.seed(seed,kinds)
for (i in 2:tr) y <- c(y, rep(TR[i], r[i]))
	trat <- sample(y, length(y), replace = FALSE)
	plots <- number+1:length(trat)-1
	dca<-data.frame(plots, trat)
	dca[,1]<-as.numeric(dca[,1])
	xx<-dca[order(dca[,2],dca[,1]),]
	r1<-seq(1,r[1])
for (i in 2:length(r)) {
	r1<-c(r1,seq(1,r[i]))
}
yy<-data.frame(xx,r=r1)
book<-yy[order(yy[,1]),]
names(book)[2]<-c(paste(deparse(substitute(trt))))
return(book)
}

