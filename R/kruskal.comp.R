"kruskal.comp" <-
function(y,trt,ties=0,alpha=0.05,main=NULL) {
nombre<-as.character(unique(trt))
t <-length(nombre)
tt <- length(trt)
tr<-1:tt
for( i in 1:tt) {
for( j in 1:t) {
if (trt[i]==nombre[j]) tr[i]<-j
}
}
trt<-tr
cat("\nStudy:",main,"\n")
cat("List of treatments\n-------------------------\n")
for(k in 1:t){
cat(k," : ",nombre[k],"\n")
}
v <- data.frame(trt, y)
m <- dim(v)
v[, 2] <- rank(v[, 2])
# Suma de rangos por tratamiento
s1<-by(v[,2],v[,1],function(x) sum(x))
s2<-as.matrix(s1)
# Numero de observaciones por tratamiento
n<-data.frame(c(by(v,v[,1],function(x) dim(x))))
ds<-dim(n)
N<-sum(n[1,])
t<-ds[2]
rs<-0
U<-0
for (i in 1:t) {rs<-rs+s2[i,1]^2/n[1,i]
U<-U+1/n[1,i]}
if(ties == 0) {
H<-12*rs/(N*(N+1))-3*(N+1)
S<-N*(N+1)/12
 } else {
# Para empates
 S<-(t(v[,2])%*%v[,2]-(N*(N+1)^2)/4)/(N-1)
 H<-(rs-(N*(N+1)^2)/4)/S
 }

# Impresion de resultados
cat("\nKruskal-Wallis Test's\n")
if(ties == 0) {
cat("No ties\n\n")
 } else {
# Para empates
cat("Ties\n\n")
 }
cat("Value K-W = ",H,"\n")
cat("degrees freedom = ",t-1,"\n")
p.chisq<-1-pchisq(H,t-1)
p.kw <- 1-pKruskalWallis(H,t,N,U)
cat("\n            P-value\n")
cat("chi.test = ",p.chisq,"\n")
cat("K-W.test = ",p.kw,"\n\n")
gl <-N-t
Tprob<-qt(1-alpha/2,gl)
#
comb <-combn(t,2)
nn<-ncol(comb)
diff<-array(0,nn)
LSD<-array(0,nn)
stat<-array(" ns  ",nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
s2[i,1]/n[1,i]
diff[k]<-abs(s2[i,1]/n[1,i]-s2[j,1]/n[1,j])
LSD[k]<-Tprob*sqrt(S*((N-1-H)/(N-t))*(1/n[1,i]+1/n[1,j]))
if (diff[k] >= LSD[k]) stat[k]<-" *   " 
}
tcomb<-t(comb)
tstat<-cbind(stat)
tr.comp<-cbind(diff)
tLSD<-cbind(LSD)
#...............
cat("\nSignificant\n")
cat("level: ",alpha,"\n")
cat("mean of the ranks\n\n")
prom<-array(0,t)
for(k in 1:t){
prom[k] <-s2[k,1]/n[1,k]
cat(k," : ",prom[k],"\n")
}

cat("\nComparison between treatment means\ntrt.  Sign   Diff    LSD\n-------------------------\n")

# imprime la significacion entre tratamientos
for(k in 1:nn){
b <-formatC(tr.comp[k],width=6,digits=4)
d <-formatC(LSD[k],width=6,digits=4)
c <-c(tstat[k])
cat(c(tcomb[k,1]),"-",c(tcomb[k,2]),c,b,d,"\n")
}
comparison <- data.frame(tcomb,tr.comp,tstat,tLSD)
return(comparison)
}

