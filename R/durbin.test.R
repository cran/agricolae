`durbin.test` <-
function(judge,trt,evaluation,alpha=0.05, group=TRUE,main=NULL) {
name.y <- paste(deparse(substitute(evaluation)))
name.t <- paste(deparse(substitute(trt)))
judge<-as.factor(judge)
trt<-as.factor(trt)
k <-unique(table(judge))
r <-unique(table(trt))
b <-nlevels(judge)
ntr <-nlevels(trt)
lambda<-r*(k-1)/(ntr-1)
x<-data.frame(judge,trt,evaluation)
# Determina el rango dentro de cada juez
z <- by(x,x$judge,function(x) rank(x$evaluation))
y<-data.frame(c(z))
m<-dim(y)
n<-m[1]*m[2]
rango <- 1:n
for (i in 1:m[1]) {
for (j in 1:m[2]) {
kk=i+m[1]*(j-1)
rango[kk]<-y[i,j]
}
}
x<-data.frame(x,rango)
means <- tapply.stat(x[,4],x[,2],stat="sum")  # change
#sds <-   tapply.stat(x[,2],x[,4],stat="sd")
#means<-data.frame(means,std.sum=sds[,2]*sqrt(r))
names(means)[1:2]<-c(name.t,name.y)
z <-by(x,x$trt,function(x) sum(x$rango))
y<-as.vector(c(z))
name<-as.character(dimnames(z)$"x$trt")
s <- (y-r*(k+1)/2)^2
s1 <- sum(s)
# determina el valor de Durbin
gl1<-ntr-1 ;gl2<-b*k-ntr-b+1
C <- b*k*(k+1)^2/4
A <- sum(rango^2)
s <- (ntr - 1) * s1/(A-C)
prob<-1-pchisq(s,gl1); Tprob<-qt(1-alpha/2,gl2)
sdtdif <- sqrt(2*r*(A-C)*(1-s/(b*(k-1)))/gl2)
LSD <-Tprob*sdtdif
# s,prob,Tprob,Mc,gl1,gl2)
# Impresion de resultados
cat("\nStudy:",main,"\n")
cat("\nSum of ranks\n")
print(data.frame( row.names=NULL,means))
cat("\nDurbin Test")
cat("\n===========")
cat("\nValue      :",s)
cat("\nDf 1       :",gl1)
cat("\nP-value    :",prob)
cat("\nAlpha      :",alpha)
cat("\nDf 2       :",gl2)
cat("\nt-Student  :",Tprob)
cat("\n\nLeast Significant Difference\nbetween the sum of ranks: ",LSD,"\n")
# comparacion de tratamientos.
cat("\nParameters BIB")
cat("\nLambda     :",lambda)
cat("\ntreatmeans :",ntr)
cat("\nBlock size :",k)
cat("\nBlocks     :",b)
cat("\nReplication:",r,"\n")
if (group)
{
cat("\nGroups, Treatments and sum of the ranks\n\n")
y<-as.numeric(y)
output<-order.stat(name,y,LSD)
}
if (!group) {
comb <-combn(ntr,2)
nn<-ncol(comb)
dif<-rep(0,nn)
pvalue<-rep(0,nn)
stat<-rep("ns",nn)
for (kk in 1:nn) {
i<-comb[1,kk]
j<-comb[2,kk]
dif[kk]<-abs(y[comb[1,kk]]-y[comb[2,kk]])
pvalue[kk]<- 2*round(1-pt(dif[kk]/sdtdif,gl2),4)
if (dif[kk] >= LSD) stat[kk]<-"*"
}
tr.i<-comb[1,]
tr.j<-comb[2,]
cat("\nComparison between treatments sum of the ranks\n\n")
print(data.frame(row.names=NULL,tr.i,tr.j,diff=dif,pvalue=pvalue,signf=stat))
output<-data.frame(means,M="",N=r)
}
#
return(output)
}
