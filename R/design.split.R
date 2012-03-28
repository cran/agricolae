design.split<-function (trt1, trt2,r=NULL, design=c("rcbd","crd","lsd"),number = 1, seed = 0, kinds = "Super-Duper",first=FALSE ) 
{
    n1<-length(trt1)
    n2<-length(trt2)
     if (seed != 0) 
        set.seed(seed, kinds)
    design <- match.arg(design)
    if (design == "crd") {
        book<-design.crd(trt1,r,number, seed, kinds)
        k<-3
        }
    if (design == "rcbd"){
        book<-design.rcbd(trt1,r,number, seed, kinds, first)
        k<-3
        }
    if (design == "lsd") {
        book<-design.lsd(trt1,number, seed, kinds, first)
        r<-n1
        k<-4
        }
nplot<-nrow(book)
d<-NULL
for(i in 1:nplot)d<-rbind(d,sample(trt2,n2))
aa<-data.frame(book,trt2=d[,1])
for(j in 2:n2) aa<-rbind(aa,data.frame(book,trt2=d[,j]))
aa<-aa[order(aa[,1]),]
    book <- aa
rownames(book)<-1:(nrow(book))
    names(book)[k] <- c(paste(deparse(substitute(trt1))))
    names(book)[k+1] <- c(paste(deparse(substitute(trt2))))
    return(book)
}


