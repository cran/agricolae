`kruskal` <-
function(y, trt,alpha=0.05,p.adj = c("none","holm", "hochberg", 
 "bonferroni", "BH", "BY", "fdr"),group=TRUE,main=NULL) {
name.y <- paste(deparse(substitute(y)))
name.t <- paste(deparse(substitute(trt)))
p.adj <- match.arg(p.adj)
junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
N<- nrow(junto)
junto[, 1] <- rank(junto[, 1])
means <- tapply.stat(junto[,1],junto[,2],stat="sum")  #change
sds <-   tapply.stat(junto[,1],junto[,2], stat="sd")  #change
nn <-   tapply.stat(junto[,1],junto[,2],stat="length") #change
means<-data.frame(means,replication=nn[,2])
names(means)[1:2]<-c(name.t,name.y)
# row.names(means)<-means[,1]
ntr<-nrow(means)
nk <- choose(ntr, 2)
DFerror<-N - ntr
    rs<- 0
    U <- 0
    for (i in 1:ntr) {
        rs <- rs + means[i, 2]^2/means[i, 3]
        U <- U + 1/means[i, 3]
    }
        S <- (sum(junto[, 1]^2) - (N * (N + 1)^2)/4)/(N - 1)
        H <- (rs - (N * (N + 1)^2)/4)/S
    cat("\nStudy:",main)
    cat("\nKruskal-Wallis test's\nTies or no Ties\n")
    cat("\nValue:", H)
    cat("\ndegrees of freedom:", ntr - 1)
    p.chisq <- 1 - pchisq(H, ntr - 1)
    cat("\nPvalue chisq  :", p.chisq,"\n\n")
    DFerror <- N - ntr
    Tprob <- qt(1 - alpha/2, DFerror)
    MSerror <- S * ((N - 1 - H)/(N - ntr))
#cat("\nComparison of treatments")
#...............
means[,2]<- means[, 2]/means[, 3]
cat(paste(name.t,",",sep="")," means of the ranks\n\n")
print(data.frame(row.names = means[,1], means[,-1]))
    if (p.adj != "none")
        {
cat("\nP value adjustment method:", p.adj)
        a <- 1e-06
        b <- 1
        for (i in 1:100) {
            x <- (b + a)/2
            xr <- rep(x,nk)
            d <- p.adjust(xr, p.adj)[1] - alpha
            ar <- rep(a,nk)
            fa <- p.adjust(ar, p.adj)[1] - alpha
            if (d * fa < 0)
                b <- x
            if (d * fa > 0)
                a <- x
        }
        Tprob <- qt(1 - x/2, DFerror)
    }
nr <- unique(means[,3])
if (group) {
Tprob<-qt(1-alpha/2,DFerror)
cat("\nt-Student:", Tprob)
cat("\nAlpha    :",alpha)
    if (length(nr) == 1) {
        LSD <- Tprob * sqrt(2 * MSerror/nr)
        cat("\nLSD      :", LSD,"\n")
    }
    else {
         nr1 <- 1/mean(1/nn[, 2])
         LSD1 <- Tprob * sqrt(2 * MSerror/nr1)
         cat("\nLSD      :", LSD1,"\n")
         cat("\nHarmonic Mean of Cell Sizes ", nr1)
         }
cat("\nMeans with the same letter are not significantly different\n")
cat("\nGroups, Treatments and mean of the ranks\n")
output <- order.group(means[,1], means[,2], means[,3], MSerror, Tprob,std.err=sqrt(MSerror/ means[,3]))
 }
if (!group) {
comb <-combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
LCL<-dif
UCL<-dif
pvalue<-dif
sdtdif <- dif
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
if (means[i, 2] < means[j, 2]){
comb[1, k]<-j
comb[2, k]<-i
}
dif[k]<-abs(means[i,2]-means[j,2])
sdtdif[k]<- sqrt(S*((N-1-H)/(N-ntr))*(1/means[i,3]+1/means[j,3]))
pvalue[k]<- 2*round(1-pt(dif[k]/sdtdif[k],DFerror),6)
}
if (p.adj != "none")pvalue <- round(p.adjust(pvalue, p.adj),6)
LCL <- dif - Tprob*sdtdif
UCL <- dif + Tprob*sdtdif
sig<-rep(" ",nn)
for (k in 1:nn) {
if (pvalue[k] <= 0.001) sig[k]<-"***"
else  if (pvalue[k] <= 0.01) sig[k]<-"**"
else  if (pvalue[k] <= 0.05) sig[k]<-"*"
else  if (pvalue[k] <= 0.1) sig[k]<-"."
}
tr.i <- means[comb[1, ],1]
tr.j <- means[comb[2, ],1]
output<-data.frame("Difference" = dif, pvalue=pvalue,sig,LCL,UCL)
rownames(output)<-paste(tr.i,tr.j,sep=" - ")
cat("\nComparison between treatments mean of the ranks\n\n")
print(output)
output<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
}
    invisible(output)
}

