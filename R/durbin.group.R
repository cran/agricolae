"durbin.group" <-
function(judge,trt,evaluation,k,r,alpha=0.05, main=NULL) {
name.y <- paste(deparse(substitute(evaluation)))
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
name<-as.character(dimnames(z)$"x$trt")
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
cat("Calculated value   : ",s,"\n")
cat("degrees of freedom : ",gl1,"\n")
cat("P-value            : ",prob,"\n")
cat("\nComparison of treatments\n")
cat("\nSignificant level  : ",alpha)
cat("\ndegrees of freedom : ",gl2)
cat("\nt-Student          : ",Tprob)
cat("\n\nLeast Significant Difference \nbetween the sum of ranks: ",Mc,"\n")
# comparacion de tratamientos.
cat("\nGroups, Treatments and sum of the ranks\n")
y<-as.numeric(y)
output<-order.stat(name,y,Mc)
#
return(output)
}

