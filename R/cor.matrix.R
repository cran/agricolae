"cor.matrix" <-
function(table,method = c("pearson","kendall", "spearman"),alternative = "two.sided")
{
method <- match.arg(method)
nvar<-ncol(table)
estimate<-rep(0,nvar*nvar)
nombres<-names(table)
dim(estimate)<-c(nvar,nvar)
dimnames(estimate)<-list(nombres,nombres)
pvalue<-estimate
nn <- round(estimate,0)
table <-as.matrix(table)
for(i in 1:nvar){
for(j in 1:nvar){
xx<-cbind(table[,i],table[,j])
yy<-na.omit(xx)
nn[i,j]<-length(yy[,1])
x<-yy[,1]
y<-yy[,2]
if (i==j) {pvalue[i,j]=0 ; estimate[i,j]=1}
else {
corr<-correl(x,y,method=method,alternative=alternative)
estimate[i,j]<-corr$rho
pvalue[i,j]<-corr$pvalue
}
}
}
names(method)=""
estimate<-round(estimate,2)
pvalue<-round(pvalue,4)
cat("\nCorrelation Analysis\n\n")
n1<-unique(c(nn))
if(length(n1)==1)nn<-n1
lista<-list(method=method,correlation=estimate,pvalue=pvalue, n.obs=nn)
return(lista)
}

