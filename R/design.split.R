design.split<-function (trt1, trt2,r=NULL, design=c("rcbd","crd","lsd"),serie = 2, seed = 0, kinds = "Super-Duper",first=FALSE )
{
    n1<-length(trt1)
    n2<-length(trt2)
     if (seed != 0) 
        set.seed(seed, kinds)
    design <- match.arg(design)
    number<-10^serie +1
    if (design == "crd") {
        book<-design.crd(trt1,r,number, seed, kinds)
        k<-3
        }
    if (design == "rcbd"){
        book<-design.rcbd(trt1,r,serie, seed, kinds, first)
        k<-3
        }
    if (design == "lsd") {
        book<-design.lsd(trt1,serie, seed, kinds, first)
        r<-n1
        k<-4
        }
nplot<-nrow(book)
d<-NULL
for(i in 1:nplot)d<-rbind(d,sample(trt2,n2))
aa<-data.frame(book,trt2=d[,1])
for(j in 2:n2) aa<-rbind(aa,data.frame(book,trt2=d[,j]))
aa<-aa[order(aa[,1]),]
splots<-rep(gl(n2,1),nplot/n2)
book <- data.frame(plots=aa[,1],splots,aa[,-1])
rownames(book)<-1:(nrow(book))
    names(book)[k+1] <- c(paste(deparse(substitute(trt1))))
    names(book)[k+2] <- c(paste(deparse(substitute(trt2))))
    return(book)
}
