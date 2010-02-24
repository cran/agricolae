`scheffe.test` <-
function (y, trt, DFerror, MSerror, Fc, alpha=0.05, group=TRUE,main = NULL)
{
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    clase<-c("aov","lm")
    if("aov"%in%class(y) | "lm"%in%class(y)){
    A<-y$model
    DFerror<-df.residual(y)
    MSerror<-deviance(y)/DFerror
    Fc<-anova(y)[trt,4]
    y<-A[,1]
    ipch<-pmatch(trt,names(A))
    if( is.na(ipch)) return(cat("Name: ",trt,"\n",names(A)[-1],"\n"))
    name.t <-names(A)[ipch]
    trt<-A[,ipch]
    name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
    sds <-   tapply.stat(junto[,1],junto[,2],stat="sd") #change
    nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
    means<-data.frame(means,std.err=sds[,2]/sqrt(nn[,2]),replication=nn[,2])
    names(means)[1:2]<-c(name.t,name.y)
#   row.names(means)<-means[,1]
    ntr<-nrow(means)
    Tprob <- qf(1-alpha,ntr-1, DFerror)
    nr<- 1/mean(1/nn[,2])
    cat("\nStudy:", main)
    cat("\n\nScheffe Test for",name.y,"\n") 
    cat("\nMean Square Error  :",MSerror,"\n\n")
    cat(paste(name.t,",",sep="")," means\n\n")
    print(data.frame(row.names = means[,1], means[,-1]))
cat("\nalpha:",alpha,"; Df Error:",DFerror,"\n")
cat("Critical Value of F:", Tprob,"\n")
scheffe <- sqrt(Tprob*(ntr-1)*2*MSerror/nr)
if (length(unique(nn[,2]))!=1) {
cat("\nHarmonic Mean of Cell Sizes ", nr )
}
if (group) {
cat("\nMinimum Significant Difference:",scheffe,"\n")
cat("\nMeans with the same letter are not significantly different.")
cat("\n\nGroups, Treatments and means\n")
output <- order.group(means[,1], means[,2], means[,4], MSerror, Tprob,means[,3], parameter=1)
}
else {
comb <-combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
sig<-NULL
LCL<-dif
UCL<-dif
pvalue<-rep(0,nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
if (means[i, 2] < means[j, 2]){
comb[1, k]<-j
comb[2, k]<-i
}
dif[k]<-abs(means[i,2]-means[j,2])
sdtdif<-sqrt(MSerror * (1/means[i,4] + 1/means[j,4]))
pvalue[k]<- round(1-pf(dif[k]^2/((ntr-1)*sdtdif^2),ntr-1,DFerror),6)

LCL[k] <- dif[k] - sqrt(Tprob*(ntr-1))*sdtdif
UCL[k] <- dif[k] + sqrt(Tprob*(ntr-1))*sdtdif
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
cat("\nComparison between treatments means\n\n")
print(output)
output<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,4],std.err=means[,3])
}
invisible(output)
}

