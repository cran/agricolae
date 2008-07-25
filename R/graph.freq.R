`graph.freq` <-
function (x, breaks="sturges",counts=NULL,frequency=1, plot=TRUE, nclass=NULL,xlab="",ylab="",...)
{
if (xlab=="") xlab= deparse(substitute(x))
if (is.numeric(x) & is.null(counts)) {
# histogram
if (is.null(nclass)) {
if (length(breaks)==1) {
x<-na.omit(x)
if(breaks== "sturges") breaks <- sturges.freq(x)$breaks
}
}
else {
amplitud <- max(x)-min(x)
n <- length(x)
z<-rep(0,n)
y<- as.character(x)
for (i in 1:n) {
lc<-nchar(y[i])
nd<-0
for (j in 1:lc) {
a <- substr(y[i],j,j)
if(a!=".")nd=nd+1
else break
}
z[i]<- lc-nd-1
}
d<-max(z)
if(d<0) d=0
tic <- round(amplitud/nclass + 0.5 * 10^(-d), d)
breaks <-seq(min(x), by=tic, length=nclass+1)
}

k<-length(breaks)
n<- length(x)
counts <- rep(0,k-1)
for (i in 1:n) {
for (j in 1:(k-1)) {
if( (x[i] >= breaks[j]) && (x[i] < breaks[j + 1])) counts[j]<-counts[j]+1
}
}
    k <- length(counts)
    mids <- rep(0, k)
    ancho <- rep(0, k)
    for (i in 1:k) {
        mids[i] <- (breaks[i] + breaks[i + 1])/2
        ancho[i] <- (breaks[i + 1] - breaks[i])
    }
    altura <- round(1.1 * max(counts), 0)
}
#############
else  {
if( is.list(x)) {
breaks<- x$breaks
counts <- x$counts
}
else breaks <- x
k<-length(counts)
mids<-rep(0,k)
ancho<-rep(0,k)
for (i in 1:k) {
mids[i]<-(breaks[i]+breaks[i+1])/2
ancho[i]<-(breaks[i+1]-breaks[i])
}
}
################
a<-breaks[1]-ancho[1]/2
b<-breaks[k+1]+ancho[k]/2
relative<-round(counts/sum(counts),4)
density <- relative/ancho
histogram<-list(breaks=breaks,counts=counts,mids=mids,relative=relative,density=density)

if(plot) {
x <- c(a, b)
if(frequency==1)height<-round(1.1*max(counts),1)
if(frequency==2)height<-round(1.1*max(relative),4)
if(frequency==3)height<-round(1.1*max(density),4)
y <- c(0, height)
suppressWarnings(warning(plot(x,y, type = "n", xlab=xlab,ylab=ylab,...)))
if (frequency==1) {
for (j in 1:k) {
suppressWarnings(warning(rect(breaks[j], 0, breaks[j + 1], counts[j], ...)))
}}
if (frequency==2) {
for (j in 1:k) {
suppressWarnings(warning(rect(breaks[j], 0, breaks[j + 1], relative[j], ...)))
}}
if (frequency==3) {
for (j in 1:k) {
suppressWarnings(warning(rect(breaks[j], 0, breaks[j + 1], density[j], ...)))
}}
}
    return(histogram)
}
