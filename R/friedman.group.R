"friedman.group" <-
function(judge,evaluation,alpha=0.05,main=NULL){
name.y <- paste(deparse(substitute(evaluation)))
row.names(evaluation)<-judge
matriz <-as.matrix(evaluation)
name<-as.character(colnames(matriz))
t <-length(name)
m<-dim(matriz)
v<-array(0,m)
for (i in 1:m[1]){
v[i,]<-rank(matriz[i,])
}
s<-array(0,m[2])
# Suma de rangos por tratamiento
for (j in 1:m[2]){
s[j]<-sum(v[,j])
}
rs<-array(0,m[2])
rs<-s-m[1]*(m[2]+1)/2
T1<-12*t(rs)%*%rs/(m[1]*m[2]*(m[2]+1))
T2<-(m[1]-1)*T1/(m[1]*(m[2]-1)-T1)
p.value<-1-pchisq(T1,m[2]-1)
# Impresion de resultados
cat("\nStudy:",main)
cat("\nFriedman's Test")
cat("\n===============\n")
#cat("\nSin empates")
#cat("\nChi Cuadrado:\t",T1)
#cat("\nP-valor     :\t",p.value,"\n")
# Para empates
A1<-0
for (i in 1:m[1]) A1 <- A1 + t(v[i,])%*%v[i,]
gl <-(m[1]-1)*(m[2]-1)
Tprob<-qt(1-alpha/2,gl)
#
LSD<-as.numeric(Tprob*sqrt(2*(m[1]*A1-t(s)%*%s)/gl))
C1 <-m[1]*m[2]*(m[2]+1)^2/4
T1.aj <-(m[2]-1)*(t(s)%*%s-m[1]*C1)/(A1-C1)
T2.aj <-(m[1]-1)*T1.aj/(m[1]*(m[2]-1)-T1.aj)
p.value<-1-pchisq(T1.aj,m[2]-1)
#cat("\nCon empates")
cat("Chi-squard : ",T1.aj,"\n")
cat("P-value    : ",p.value)
#...............
cat("\nSignificant\n")
cat("level      : ",alpha,"\n")
cat("\nt-Student  :\t",Tprob)

#cat("\nRepetición:\t",nr)
cat("\nLSD        :\t",LSD)
cat("\n\nComparison between treatments\n\nGroup, Treatment and Sum of the ranks\n")
s<-as.numeric(s)
output<-order.stat(name,s,LSD)
#
return(output)
}

