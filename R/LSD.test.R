"LSD.test" <-
function (y, trt, DFerror, MSerror, alpha=0.05, group=TRUE,main = NULL)
{
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[,2],junto[,1],mean)
    sds <-   tapply.stat(junto[,2],junto[,1],sd)
    nn <-   tapply.stat(junto[,2],junto[,1],length)
    means<-data.frame(means,std.err=sds[,2]/sqrt(nn[,2]),replication=nn[,2])
    names(means)[1:2]<-c(name.t,name.y)
    row.names(means)<-means[,1]
    ntr<-nrow(means)
    Tprob<-qt(1-alpha/2,DFerror)
    nr <- unique(nn[,2])
nfila<-c("Alpha", "Error Degrees of Freedom", "Error Mean Square",
"Critical Value of t")
nvalor<-c( alpha,  DFerror, MSerror, Tprob)
    cat("\nStudy:", main)
    cat("\n\nLSD t Test for",name.y,"\n")
xtabla<-data.frame("......"=nvalor)
row.names(xtabla)<-nfila
print(xtabla)
cat("\nTreatment Means\n")
print(data.frame( row.names=NULL,means))
if (group) {
    if (length(nr) == 1) {
        LSD <- Tprob * sqrt(2 * MSerror/nr)
        cat("\nLeast Significant Difference", LSD)
    }
    else {
         nr1<- 1/mean(1/nn[,2])
        LSD1 <- Tprob * sqrt(2 * MSerror/nr1)
        cat("\nLeast Significant Difference", LSD1)
        cat("\nHarmonic Mean of Cell Sizes ", nr1 )
        cat("\n\nDifferent LSD for each comparison")
        }
cat("\nMeans with the same letter are not significantly different.")
cat("\n\nGroups, Treatments and means\n")
output <- order.group(means[,1], means[,2], means[,4], MSerror, Tprob,means[,3])
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
sdtdif<-sqrt(MSerror * (1/means[i,4] + 1/means[j,4]))
pvalue[k]<- 2*round(1-pt(dif[k]/sdtdif,DFerror),4)
}

tr.i<-comb[1,]
tr.j<-comb[2,]

cat("\nComparison between treatments means\n\n")
print(data.frame(row.names=NULL,tr.i,tr.j,diff=dif,pvalue=pvalue))

output<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,4],std.err=means[,3])
}
return(output)
}
