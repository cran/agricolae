design.strip<-function (trt1, trt2,r, number = 1, seed = 0, kinds = "Super-Duper") 
{
    n1<-length(trt1)
    n2<-length(trt2)
    if (seed != 0) 
        set.seed(seed, kinds)
        a<-sample(trt1,n1)
        b<-sample(trt2,n2)
        fila<-rep(b,n1)
        columna <- a[gl(n1,n2)]
        block <- rep(1,n1*n2)
    if (r > 1) {
    for (i in 2:r) {
        a<-sample(trt1,n1)
        b<-sample(trt2,n2)
        fila<-c(fila,rep(b,n1))
        columna <- c(columna,a[gl(n1,n2)])
        block <- c(block,rep(i,n1*n2))
    }}
    plots <- number + 1:(n1*n2 * r) - 1
    book <- data.frame(plots, block = as.factor(block), column=as.factor(columna),row = as.factor(fila))
    names(book)[3] <- c(paste(deparse(substitute(trt1))))
    names(book)[4] <- c(paste(deparse(substitute(trt2))))
    return(book)
}
