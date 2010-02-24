`waller.test` <-
function (y, trt, DFerror, MSerror, Fc, K = 100, group=TRUE,main = NULL)
{
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    clase<-c("aov","lm")
    if("aov"%in%class(y) | "lm"%in%class(y)){
    A<-y$model
    DFerror<-df.residual(y)
    MSerror<-deviance(y)/DFerror
    ipch<-pmatch(trt,names(A))
    if( is.na(ipch)) return(cat("Name: ",trt,"\n",names(A)[-1],"\n"))
    name.t <-names(A)[ipch]
    Fc<-anova(y)[trt,4]
    trt<-A[,ipch]
    y<-A[,1]
    name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd")   # change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    means<-data.frame(means,std.err=sds[,2]/sqrt(nn[,2]),replication=nn[,2])
    names(means)[1:2]<-c(name.t,name.y)
#    row.names(means)<-means[,1]
    ntr<-nrow(means)
    Tprob <- waller(K,ntr-1,DFerror,Fc)
    nr <- unique(nn[,2])
nfila<-c("K ratio", "Error Degrees of Freedom", "Error Mean Square","F value",
"Critical Value of Waller")
nvalor<-c( K,  DFerror, MSerror, Fc, Tprob)
    cat("\nStudy:", main)
    cat("\n\nWaller-Duncan K-ratio t Test for",name.y,"\n")
    cat("\nThis test minimizes the Bayes risk under additive")
    cat("\nloss and certain other assumptions.\n")
xtabla<-data.frame("......"=nvalor)
row.names(xtabla)<-nfila
print(xtabla)
cat("\n")
cat(paste(name.t,",",sep="")," means\n\n")
print(data.frame(row.names = means[,1], means[,-1]))
    if (length(nr) == 1) {
        MSD <- Tprob * sqrt(2 * MSerror/nr)
        cat("\nMinimum Significant Difference", MSD)
    }
    else {
        cat("\nDifferent MSD for each comparison")
    }

if (group) {
cat("\nMeans with the same letter are not significantly different.")
cat("\n\nComparison of treatments\n\nGroups, Treatments and means\n")
output <- order.group(means[,1], means[,2], means[,4], MSerror, Tprob,means[,3])
}
if (!group) {
comb <-combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
MSD1<-rep(0,nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
if (means[i, 2] < means[j, 2]){
comb[1, k]<-j
comb[2, k]<-i
}
dif[k]<-abs(means[i,2]-means[j,2])
MSD1[k]<-Tprob*sqrt(MSerror * (1/means[i,4] + 1/means[j,4]))
}
tr.i <- means[comb[1, ],1]
tr.j <- means[comb[2, ],1]
if (length(nr) == 1)  {
significant = dif > MSD
output<-data.frame("Difference" = dif, significant)
}
else  {
significant = dif > MSD1
output<-data.frame("Difference" = dif, MSD=MSD1,significant)
}
rownames(output)<-paste(tr.i,tr.j,sep=" - ")
cat("\nComparison between treatments means\n\n")
print(output)
output<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,4],std.err=means[,3])
}
invisible(output)
}

