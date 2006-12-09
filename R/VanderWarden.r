"waerden" <-
function(y,trt,alpha=0.05,main=NULL) {
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
    N<- length(v[,2])
    v[, 2] <- qnorm(round(rank(v[, 2]) /(N+1),3))
    s1 <- by(v[, 2], v[, 1], function(x) mean(x))
    s2 <- as.matrix(s1)
    s3 <- by( v[, 2], v[, 1], function(x) sd(x)/sqrt(length(x)))
    s4 <- as.matrix(s3)
    S <- sum(v[, 2]^2)/(N-1)
    n <- data.frame(c(by(v, v[, 1], function(x) dim(x))))

    ds <- dim(n)
#N<-sum(n[1,])
t<-ds[2]
#rs<-0
    T1 <- 0
    for (i in 1:k) {
        T1 <- T1 + s2[i, 1]^2*n[1, i]
    }
    T1<-T1/S

# Impresion de resultados
    cat("\nStudy:",main)
    cat("\nVan der Waerden (Normal Scores) test's\n")
    cat("\nValue             : ", T1)
    cat("\ndegrees of freedom: ", k - 1)
    p.chisq <- 1 - pchisq(T1, k - 1)
    cat("\n\nP.value")
    cat("\nchisq.test : ", p.chisq, "\n")
    gl <- N - t
    Tprob <- qt(1 - alpha/2, gl)
#
comb <-combn(t,2)
nn<-ncol(comb)
diff<-array(0,nn)
LSD<-array(0,nn)
stat<-array(" ns  ",nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]
s2[i,1]
diff[k]<-abs(s2[i,1]-s2[j,1])
LSD[k]<-Tprob*sqrt(S*((N-1-T1)/(N-t))*(1/n[1,i]+1/n[1,j]))
if (diff[k] >= LSD[k]) stat[k]<-" *   " 
}
tcomb<-t(comb)
tstat<-cbind(stat)
tr.comp<-cbind(diff)
tLSD<-cbind(LSD)
#...............
cat("\nSignificant\n")
cat("level: ",alpha,"\n")
cat("mean of the normal score\n\n")
prom<-array(0,t)
for(k in 1:t){
prom[k] <-s2[k,1]
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

