"durbin.comp" <-
function(judge,trt,evaluation,k,r,alpha=0.05,main=NULL) {
x<-data.frame(judge,trt,evaluation)
t<-length(unique(x$trt))
b<-length(unique(x$judge))
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
z <-by(x,x$trt,function(x) sum(x$rango))
y<-as.vector(c(z))
s <- (y-r*(k+1)/2)^2
s1 <- sum(s)
# determina el valor de Durbin
gl1<-t-1 ;gl2<-b*k-t-b+1
s <- 12*(t-1)*s1/(r*t*(k-1)*(k+1))
prob<-1-pchisq(s,gl1); Tprob<-qt(1-alpha/2,gl2)
Mc <-Tprob*sqrt( r*k*(k+1)*(b*(k-1)-s)/(6*gl2))

# s,prob,Tprob,Mc,gl1,gl2)
# Impresion de resultados
cat("\nStudy:",main,"\n")
cat("Durbin Test\n\n")
cat("Calculated value   = ",s,"\n")
cat("degrees of freedom = ",gl1,"\n")
cat("P-value            = ",prob,"\n\n")
cat("t-Student          = ",Tprob,"\n")
cat("degrees of freedom = ",gl2,"\n\n")
cat("Least Significant Difference \nbetween the sum of ranks = ",Mc,"\n\n")

# comparacion de tratamientos.
comb <-combn(t,2)
n<-ncol(comb)
diff<-array(0,n)
stat<-array(" ns  ",n)
for (kk in 1:n) {
diff[kk]<-abs(y[comb[1,kk]]-y[comb[2,kk]])
if (diff[kk] >= Mc) stat[kk]<-" *   " 
}
tcomb<-t(comb)
tstat<-cbind(stat)
trt.comp<-cbind(diff)
cat("sum of ranks\ntrt   Sum\n----------\n")
for(i in 1:t){
a <-c(i)
b <-c(" = ",y[i])
cat(a,b,"\n")
}
Comparison <- data.frame(tcomb,trt.comp,tstat)
return(Comparison)
}

