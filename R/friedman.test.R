"friedman" <-
function(judge,trt,evaluation,alpha=0.05,group=TRUE,main=NULL){
name.x <- paste(deparse(substitute(judge)))
name.y <- paste(deparse(substitute(evaluation)))
name.t <- paste(deparse(substitute(trt)))
matriz<-suppressWarnings(mxyz(judge,trt,evaluation))
#matriz <-as.matrix(evaluation)
name<-as.character(colnames(matriz))
ntr <-length(name)
m<-dim(matriz)
v<-array(0,m)
for (i in 1:m[1]){
v[i,]<-rank(matriz[i,])
}
vv<-as.numeric(v)
junto <- data.frame(evaluation, trt)
means <- tapply.stat(junto[,2],junto[,1],mean)
sds <-   tapply.stat(junto[,2],junto[,1],sd)
nn <-   tapply.stat(junto[,2],junto[,1],length)

nr<-unique(nn[,2])
s<-array(0,m[2])
# Suma de rangos por tratamiento
for (j in 1:m[2]){
s[j]<-sum(v[,j])
}
means<-data.frame(means,replication=nn[,2])
means[,2]<-s
names(means)[1:2]<-c(name.t,name.y)
row.names(means)<-means[,1]
rs<-array(0,m[2])
rs<-s-m[1]*(m[2]+1)/2
T1<-12*t(rs)%*%rs/(m[1]*m[2]*(m[2]+1))
T2<-(m[1]-1)*T1/(m[1]*(m[2]-1)-T1)
# Impresion de resultados
cat("\nStudy:",main)
cat("\n\nSum of the ranks\n")
print(data.frame( row.names=NULL,means))
cat("\nFriedman's Test")
cat("\n===============")
A1<-0
for (i in 1:m[1]) A1 <- A1 + t(v[i,])%*%v[i,]
DFerror <-(m[1]-1)*(m[2]-1)
Tprob<-qt(1-alpha/2,DFerror)
#
LSD<-as.numeric(Tprob*sqrt(2*(m[1]*A1-t(s)%*%s)/DFerror))
C1 <-m[1]*m[2]*(m[2]+1)^2/4
T1.aj <-(m[2]-1)*(t(s)%*%s-m[1]*C1)/(A1-C1)
T2.aj <-(m[1]-1)*T1.aj/(m[1]*(m[2]-1)-T1.aj)
p.value<-1-pchisq(T1.aj,m[2]-1)
p.fried<-1-pFriedman(T1.aj, ntr, nr)
cat("\nChi-squard:",T1.aj)
cat("\nPvalue    :",p.value)
cat("\npFriedman :",p.fried)
cat("\nAlpha     :",alpha)
cat("\nt-Student :",Tprob)
#...............
#cat("\nReplication:\t",nr)
if (group) {
cat("\nLSD       :",LSD)
cat("\n\nMeans with the same letter are not significantly different.")
cat("\nGroup, Treatment and Sum of the ranks\n")
s<-as.numeric(s)
output<-order.stat(name,s,LSD)
names(output)[2]<-"Sum of ranks"
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
dif[k]<-abs(s[comb[1,k]]-s[comb[2,k]])
sdtdif<- sqrt(2*(m[1]*A1-t(s)%*%s)/DFerror)
pvalue[k]<- 2*round(1-pt(dif[k]/sdtdif,DFerror),4)
LSD[k]<-round(Tprob*sdtdif,2)
if (dif[k] >= LSD[k]) stat[k]<-"*" 
}
tr.i<-comb[1,]
tr.j<-comb[2,]
cat("\n\nComparison between treatments\nSum of the ranks\n\n")
print(data.frame(row.names=NULL,tr.i,tr.j,diff=dif,pvalue=pvalue,signf=stat,LSD=LSD))

output<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
}
return(output)
}
