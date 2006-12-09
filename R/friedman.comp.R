"friedman.comp" <-
function(judge,evaluation,alpha=0.05,main=NULL){
row.names(evaluation)<-judge
matriz <-as.matrix(evaluation)
nombre<-colnames(matriz)
cat("\nStudy:",main)
cat("\nFriedman's Test\n")
cat("\n===============\n")
t <-length(nombre)
cat("List of treatments\n-------------------------\n")
for(k in 1:t){
cat(k," : ",nombre[k],"\n")
}
cat("-------------------------\n")
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
#cat("Sin empates\n\n")
#cat("Chi-cuadrado = ",T1,"\n")
#cat("P-valor = ",p.value,"\n\n")

# Para empates
A1<-0
for (i in 1:m[1]) A1 <- A1 + t(v[i,])%*%v[i,]
gl <-(m[1]-1)*(m[2]-1)
Tprob<-qt(1-alpha/2,gl)
#
LSD<-Tprob*sqrt(2*(m[1]*A1-t(s)%*%s)/gl)
C1 <-m[1]*m[2]*(m[2]+1)^2/4
T1.aj <-(m[2]-1)*(t(s)%*%s-m[1]*C1)/(A1-C1)
T2.aj <-(m[1]-1)*T1.aj/(m[1]*(m[2]-1)-T1.aj)
p.value<-1-pchisq(T1.aj,m[2]-1)
#cat("Con empates\n\n")
cat("Chi-squard : ",T1.aj,"\n")
cat("P-value    : ",p.value,"\n")
cat("Significant\n")
cat("level      : ",alpha,"\n")
comb <-combn(m[2],2)
n<-ncol(comb)
diff<-array(0,n)
stat<-array(" ns  ",n)
for (k in 1:n) {
diff[k]<-abs(s[comb[1,k]]-s[comb[2,k]])
if (diff[k] >= LSD) stat[k]<-" *   " 
}
tcomb<-t(comb)
tstat<-cbind(stat)
trt.comp<-cbind(diff)
cat("\nSum of ranks\n\Trt    Sum\n----------\n")
for(i in 1:m[2]){
a <-c(i)
b <-c(" = ",s[i])
cat(a,b,"\n")
}
cat("\nComparison between treatments\nTrt   Sign Diff\n----------------\n")
# imprime la significacion entre tratamientos
for(i in 1:n){
a <-c(tcomb[i,1])
b <-c(trt.comp[i])
c <-c(tstat[i])
cat(c(tcomb[i,1]),"-",c(tcomb[i,2]),c,b,"\n")
}
Comparison<- data.frame(tcomb,trt.comp,tstat)
return(Comparison)
}

