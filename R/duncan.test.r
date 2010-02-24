`duncan.test` <-
function (y, trt, DFerror, MSerror, alpha=0.05, group=TRUE,main = NULL)
{
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    clase<-c("aov","lm")
    if("aov"%in%class(y) | "lm"%in%class(y)){
    A<-y$model
    DFerror<-df.residual(y)
    MSerror<-deviance(y)/DFerror
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
    Tprob <- qtukey((1-alpha)^(1:(ntr-1)),2:ntr, DFerror)
    nr <- unique(nn[,2])

#"Critical Value of Studentized Range")

    cat("\nStudy:", main)
    cat("\n\nDuncan's new multiple range test\nfor",name.y,"\n")
    cat("\nMean Square Error: ",MSerror,"\n\n")
    cat(paste(name.t,",",sep="")," means\n\n")
    print(data.frame(row.names = means[,1], means[,-1]))
if(length(nr) == 1 ) sdtdif <- sqrt(MSerror/nr)
else {
nr1 <-  1/mean(1/nn[,2])
sdtdif <- sqrt(MSerror/nr1)
}
DUNCAN <- Tprob * sdtdif
names(DUNCAN)<-2:ntr
cat("\nalpha:",alpha,"; Df Error:",DFerror,"\n")
cat("\nCritical Range\n")
print(DUNCAN)
if (length(nr) > 1) {
cat("\nHarmonic Mean of Cell Sizes ", nr1 )
cat("\n\nDifferent value for each comparison")
}
if (group) {
cat("\nMeans with the same letter are not significantly different.")
cat("\n\nGroups, Treatments and means\n")
output <- order.group(means[,1], means[,2], means[,4], MSerror, 1 ,means[,3], parameter=0.5,snk=2,DFerror,alpha,sdtdif)
}
if (!group) {
Omeans<-order(means[,2],decreasing = TRUE)
Ordindex<-order(Omeans)
comb <-combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
LCL<-dif
UCL<-dif
pvalue<-dif
odif<-dif
sig<-NULL
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
if (means[i, 2] < means[j, 2]){
comb[1, k]<-j
comb[2, k]<-i
}
dif[k]<-abs(means[i,2]-means[j,2])
nx<-abs(i-j)+1
odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
#sdtdif<-sqrt(MSerror * (1/means[i,4] + 1/means[j,4]))
pvalue[k]<- round((1-ptukey(dif[k]/sdtdif,odif[k],DFerror))^1/(odif[k]-1),6)
LCL[k] <- dif[k] - DUNCAN[odif[k]-1]
UCL[k] <- dif[k] + DUNCAN[odif[k]-1]
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

