`waerden.test` <-
function(y, trt,alpha=0.05,group=TRUE,main=NULL) {
name.y <- paste(deparse(substitute(y)))
name.t <- paste(deparse(substitute(trt)))
junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
N<- nrow(junto)
junto[, 1] <- qnorm(round(rank(junto[, 1]) /(N+1),3))
S <- sum(junto[,1]^2)/(N-1)
means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
means<-data.frame(means,replication=nn[,2])
names(means)[1:2]<-c(name.t,name.y)
#row.names(means)<-means[,1]
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
cat("\nDegrees of freedom: ", ntr - 1,"\n\n")
cat(paste(name.t,",",sep="")," means of the normal score\n\n")
print(data.frame(row.names = means[,1], means[,-1]))
MSerror <- S * ((N - 1 - T1)/(N - ntr))
#cat("\nComparison of treatments")
#...............

nr <- unique(means[,3])
Tprob<-qt(1-alpha/2,DFerror)
if (group) {
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
cat("\nGroups, Treatments and means of the normal score\n")
output <- order.group(means[,1], means[,2], means[,3], MSerror, Tprob,std.err=sqrt(MSerror/ means[,3]))
 }
 if (!group) {
comb <-combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
LCL<-dif
UCL<-dif
sig<-NULL
pvalue<-rep(0,nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
#if (means[i, 2] < means[j, 2]){
#comb[1, k]<-j
#comb[2, k]<-i
#}
dif[k]<-means[i,2]-means[j,2]
sdtdif<- sqrt(S*((N-1-T1)/(N-ntr))*(1/means[i,3]+1/means[j,3]))
pvalue[k]<- 2*round(1-pt(abs(dif[k])/sdtdif,DFerror),6)
LSD <- Tprob*sdtdif
LCL[k] <- dif[k] - LSD
UCL[k] <- dif[k] + LSD
sig[k]<-" "
if (pvalue[k] <= 0.001) sig[k]<-"***"
else  if (pvalue[k] <= 0.01) sig[k]<-"**"
else  if (pvalue[k] <= 0.05) sig[k]<-"*"
else  if (pvalue[k] <= 0.1) sig[k]<-"."
}
tr.i <- means[comb[1, ],1]
tr.j <- means[comb[2, ],1]
output<-data.frame("Difference" = dif, pvalue=pvalue,sig,LCL,UCL)
rownames(output)<-paste(tr.i,tr.j,sep=" - ")
cat("\nComparison between treatments means\nmean of the normal score\n\n")
print(output)
output<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
}
    invisible(output)
}

