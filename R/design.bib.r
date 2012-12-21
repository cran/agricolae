`design.bib` <-
function (trt, k, number = 1, seed = 0, kinds = "Super-Duper")
{
    ntr <- length(trt)
    if (seed != 0) set.seed(seed, kinds)
    md<- t(combn(1:ntr, k))
    b<-nrow(md)
    bp<-sample(1:b,b)
    md<- md[bp,]
    for (i in 1:b) {
    bi<-sample(1:k,k)
    md[i,]<- md[i,bi]
    }
mtr<-trt[t(md)]
block <- gl(b,k)
plots <- number + 1:(b*k) - 1
book <- data.frame(plots, block = as.factor(block), trt = as.factor(mtr))
names(book)[3] <- c(paste(deparse(substitute(trt))))
r<-as.numeric(table(book[,3])[1])
lambda<-r*(k-1)/(ntr-1)
E<-lambda*ntr/(r*k)
cat("\nParameters BIB\n==============")
cat("\nLambda     :",lambda)
cat("\ntreatmeans :",ntr)
cat("\nBlock size :",k)
cat("\nBlocks     :",b)
cat("\nReplication:",r,"\n")
cat("\nEfficiency factor",E,"\n\n<<< Book >>>\n")
parameters<-data.frame(lambda= lambda,treatmeans=ntr,blockSize=k,blocks=b,r=r,Efficiency=E)
rownames(parameters)<-"values"
return(list(parameters=parameters,book=book))
}

