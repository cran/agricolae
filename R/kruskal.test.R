"kruskal" <-
function(y, trt,alpha=0.05,group=TRUE,main=NULL) {
name.y <- paste(deparse(substitute(y)))
name.t <- paste(deparse(substitute(trt)))
junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
N<- nrow(junto)
junto[, 1] <- rank(junto[, 1])
means <- tapply.stat(junto[,2],junto[,1],stat="sum")
sds <-   tapply.stat(junto[,2],junto[,1], stat="sd")
nn <-   tapply.stat(junto[,2],junto[,1],stat="length")
means<-data.frame(means,replication=nn[,2])
names(means)[1:2]<-c(name.t,name.y)
# row.names(means)<-means[,1]
ntr<-nrow(means)
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
    cat("\nKruskal-Wallis test's\n")
    cat("\nValue:", H)
    cat("\ndegrees of freedom:", ntr - 1)
    p.chisq <- 1 - pchisq(H, ntr - 1)
    p.kw <- 1-pKruskalWallis(H, ntr, N, U)
    cat("\nPvalue chisq  :", p.chisq)
    cat("\npKruskalWallis:", p.kw, "\n")
    DFerror <- N - ntr
    Tprob <- qt(1 - alpha/2, DFerror)
    MSerror <- S * ((N - 1 - H)/(N - ntr))
#cat("\nComparison of treatments")
#...............
cat("\nMean of the ranks\n")
means[,2]<- means[, 2]/means[, 3]
print(data.frame( row.names=NULL,means))
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
pvalue<-rep(0,nn)
LSD<-rep(0,nn)
stat<-rep("ns",nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
dif[k]<-abs(means[i,2]-means[j,2])
sdtdif<- sqrt(S*((N-1-H)/(N-ntr))*(1/means[i,3]+1/means[j,3]))
pvalue[k]<- 2*round(1-pt(dif[k]/sdtdif,DFerror),4)
LSD[k]<-round(Tprob*sdtdif,2)
if (dif[k] >= LSD[k]) stat[k]<-"*" 
}
tr.i<-comb[1,]
tr.j<-comb[2,]
cat("\nComparison between treatments mean of the ranks\n")
print(data.frame(row.names=NULL,tr.i,tr.j,diff=dif,pvalue=pvalue,signf=stat,LSD=LSD))
output<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
}
    return(output)
}
