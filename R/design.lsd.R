"design.lsd" <-
function (trt,number=1,seed=0,kinds="Super-Duper")
{
r <- length(trt)
if(seed != 0) set.seed(seed,kinds)
m <- sample(trt, r)
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

# Asigna valores de fila y columna
fila <- rep(0, r * r)
columna <- fila
trat <- fila
plots <- 0
for (i in 1:r) {
for (j in 1:r) {
plots <- plots + 1
fila[plots] <- i
columna[plots] <- j
trat[plots] <- m[a[i, j]]
}
}
plots <- number+1:length(trat)-1
book<-data.frame(plots,row=as.factor(fila),col=as.factor(columna),
trat=as.factor(trat))
names(book)[4]<-c(paste(deparse(substitute(trt))))
return(book) }

