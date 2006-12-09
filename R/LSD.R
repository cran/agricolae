"LSD" <-
function(y,trt,df,MSerror,alpha=0.05,main=NULL){
name.y <- paste(deparse(substitute(y)))
conjunto <- subset(data.frame(y,trt),is.na(y)==FALSE)
y<-conjunto$y
trt<-conjunto$trt
pro<-cbind(by(y,trt,function(x) mean(x)))
std <- cbind(by(y, trt, function(x) sd(x)))
nn<-cbind(by(y,trt,function(x) length(x)))
p <-as.vector(pro[,1])
n <-as.vector(nn[,1])
t <-length(p)
std.err<-std/sqrt(nn)
Tprob<-qt(1-alpha/2,df)
nfila<-c("Alpha", "Error Degrees of Freedom", "Error Mean Square",
"Critical Value of t")
nvalor<-c( alpha,  df, MSerror, Tprob)

    cat("\nStudy:", main)
    cat("\n\nLSD t Test for",name.y,"\n")
xtabla<-data.frame("......"=nvalor)
row.names(xtabla)<-nfila
print(xtabla)
cat("\n")
#    cat("\nTreatments:\n\n")
    nombre <- cbind(row.names(pro))
    resumen <- data.frame(treatments=1:t,N=n,means=pro,std.error=std.err)
    print(resumen)
#
comb <-combn(t,2)
comb <- cbind(comb)
nn<-ncol(comb)
diff<-array(0,nn)
lsd<-array(0,nn)
stat<-array(" ns  ",nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]

diff[k]<-abs(p[i]-p[j])
lsd[k]<-Tprob*sqrt(MSerror*(1/n[i]+1/n[j]))
if (diff[k] >= lsd[k]) stat[k]<-" *   " 
}

tcomb<-t(comb)
tstat<-cbind(stat)
tr.comp<-cbind(diff)
tLSD<-cbind(lsd)

cat("\nComparison between treatments means\n\nTrt.  Sign   Diff    LSD\n-------------------------\n")

# imprime la significacion entre tratamientos
for(k in 1:nn){
b <-formatC(tr.comp[k],width=6,digits=4)
d <-formatC(lsd[k],width=6,digits=4)
c <-c(tstat[k])
cat(c(tcomb[k,1]),"-",c(tcomb[k,2]),c,b,d,"\n")
}
#comparacion <- data.frame(tcomb,trt.comp,tstat,tLSD)
return(data.frame(tcomb,tr.comp,tstat,tLSD))
# Termina la funcion LSD
}

