`BIB.test` <-
function(block,trt,y, method="lsd", alpha=0.05,group=TRUE)
{
block.unadj<-as.factor(block)
trt.adj  <-as.factor(trt)
name.y <- paste(deparse(substitute(y)))
model<-lm(y ~ block.unadj+trt.adj)
DFerror<-df.residual(model)
MSerror<-deviance(model)/DFerror
k <-unique(table(block.unadj))
r <-unique(table(trt.adj))
b <-nlevels(block.unadj)
ntr <-nlevels(trt.adj)
lambda<-r*(k-1)/(ntr-1)
tabla<-suppressWarnings(mxyz(block,trt,y ))
AA<-!is.na(tabla)
BB<-tapply(y,block.unadj,sum)
B<-BB%*%AA
Y<-tapply(y,trt.adj,sum)
Q<-Y-as.numeric(B)/k

SStrt.adj<-sum(Q^2)*k/(lambda*ntr)
MStrt.adj<- SStrt.adj/(ntr-1)
sdtdif<-sqrt(2*k*MSerror/(lambda*ntr))
Fvalue<- MStrt.adj/MSerror
# mean adjusted.

mean.adj<-mean(y)+Q*k/(lambda*ntr)
sdmean.adj <- sqrt(MSerror*(1+k*r*(ntr-1)/(lambda*ntr))/(r*ntr))
cat("\nANALYSIS BIB: ", name.y, "\nClass level information\n")
cat("\nBlock: ", unique(as.character(block)))
cat("\nTrt  : ", unique(as.character(trt)))
cat("\n\nNumber of observations: ", length(y), "\n\n")
print(anova(model))
cat("coefficient of variation:",round(cv.model(model),1),"%\n")
cat("\nTreatments\n")
print(data.frame( row.names=NULL,trt=row.names(Y),means=Y/r,mean.adj,sdmean.adj))
parameter<- k/(lambda*ntr)
if (method=="lsd") {
Tprob<-qt(1-alpha/2,DFerror)
cat("\nLSD test\n")
cat("\nAlpha  :",alpha)
cat("\nLSD    :",Tprob*sdtdif)
}
if (method=="tukey") {
Tprob <- qtukey(1-alpha, ntr, DFerror)
cat("\nTukey\n")
cat("\nAlpha  :",alpha)
cat("\nHSD    :",Tprob*sdtdif)
parameter<-parameter/2
}
if (method=="waller") {
K<-650-16000*alpha+100000*alpha^2
Tprob<-waller(K,ntr-1,DFerror,Fvalue)
cat("\nWaller-Duncan K-ratio\n")
cat("\nThis test minimizes the Bayes risk under additive")
cat("\nloss and certain other assumptions.\n")
cat("\nk Ratio: ",K)
cat("\nMSD    :",Tprob*sdtdif)
}
E<-lambda*ntr/(r*k)
cat("\nParameters BIB")
cat("\nLambda     :",lambda)
cat("\ntreatmeans :",ntr)
cat("\nBlock size :",k)
cat("\nBlocks     :",b)
cat("\nReplication:",r,"\n")
cat("\nEfficiency factor",E,"\n\n<<< Book >>>\n")
if (group) {
cat("\nMeans with the same letter are not significantly different.")
cat("\n\nComparison of treatments\n\nGroups, Treatments and means\n")
output <- order.group(names(mean.adj), as.numeric(mean.adj), rep(1,ntr),
MSerror, Tprob,std.err=sdmean.adj,parameter)
output[,4]<-r
}
if (!group) {
comb <-combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
pvalue<-rep(0,nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
dif[k]<-abs(mean.adj[i]-mean.adj[j])
if (method=="lsd") pvalue[k]<- 2*round(1-pt(dif[k]/sdtdif,DFerror),4)
if (method=="tukey") pvalue[k]<- round(1-ptukey(dif[k]*sqrt(2)/sdtdif,ntr,DFerror),4)
}
if (method=="waller") significant = dif > Tprob*sdtdif

tr.i<-comb[1,]
tr.j<-comb[2,]

cat("\nComparison between treatments means\n")
if (method=="waller") print(data.frame(row.names=NULL,tr.i,tr.j,diff=dif,significant))
else print(data.frame(row.names=NULL,tr.i,tr.j,diff=dif,pvalue))
output<-data.frame(trt= names(mean.adj),means= as.numeric(mean.adj),M="",
N=r,std.err=sdmean.adj)
}
return(output)
}
