"waerden.test" <-
function(y, trt,alpha=0.05,group=TRUE,main=NULL) {
name.y <- paste(deparse(substitute(y)))
name.t <- paste(deparse(substitute(trt)))
junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
N<- nrow(junto)
junto[, 1] <- qnorm(round(rank(junto[, 1]) /(N+1),3))
S <- sum(junto[,1]^2)/(N-1)
means <- tapply.stat(junto[,2],junto[,1],mean)
nn <-   tapply.stat(junto[,2],junto[,1],length)
means<-data.frame(means,replication=nn[,2])
names(means)[1:2]<-c(name.t,name.y)
row.names(means)<-means[,1]
ntr<-nrow(means)
DFerror<-N - ntr
T1 <- 0
for (i in 1:ntr) {
T1 <- T1 + means[i, 2]^2*means[i,3]
}
T1<-T1/S
cat("\nStudy:",main)
cat("\nVan der Waerden (Normal Scores) test's\n")
cat("\nValue :", T1)
p.chisq <- 1 - pchisq(T1, ntr - 1)
cat("\nPvalue:", p.chisq)
cat("\nDegrees of freedom: ", ntr - 1)
cat("\n\nMeans of the normal score\n")
print(data.frame( row.names=NULL,means))
MSerror <- S * ((N - 1 - T1)/(N - ntr))
#cat("\nComparison of treatments")
#...............

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
        cat("\nMinimum difference changes for each comparison\n")
    }
cat("\nMeans with the same letter are not significantly different\n")
cat("\nGroups, Treatments and means of the normal score\n")
output <- order.group(means[,1], means[,2], means[,3], MSerror, Tprob,std.err=sqrt(MSerror/ means[,3]))
 }
 if (!group) {
comb <-combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
pvalue<-rep(0,nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
dif[k]<-abs(means[i,2]-means[j,2])
sdtdif<- sqrt(S*((N-1-T1)/(N-ntr))*(1/means[i,3]+1/means[j,3]))
pvalue[k]<- 2*round(1-pt(dif[k]/sdtdif,DFerror),4)
}
tr.i<-comb[1,]
tr.j<-comb[2,]
cat("\nComparison between treatments means\nmean of the normal score\n")
print(data.frame(row.names=NULL,tr.i,tr.j,diff=dif,pvalue=pvalue))
output<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
}
    return(output)
}
