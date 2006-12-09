"HSD" <-
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
Tprob<-qtukey(1-alpha,t,df)
nfila<-c("Alpha", "Error Degrees of Freedom", "Error Mean Square",
"Critical Value of Studentized Range")
nvalor<-c( alpha,  df, MSerror, Tprob)

    cat("\nStudy:", main)
    cat("\n\nHSD Test for",name.y,"\n")
xtabla<-data.frame("......"=nvalor)
row.names(xtabla)<-nfila
print(xtabla)
cat("\n")
    nombre <- cbind(row.names(pro))
    resumen <- data.frame(treatments=1:t,N=n,means=pro,std.error=std.err)
    print(resumen)
#
comb <-combn(t,2)
comb <- cbind(comb)
nn<-ncol(comb)
diff<-array(0,nn)
hsd<-array(0,nn)
stat<-array(" ns  ",nn)
for (k in 1:nn) {
i<-comb[1,k]
j<-comb[2,k]

diff[k]<-abs(p[i]-p[j])
hsd[k]<-Tprob*sqrt(0.5*MSerror*(1/n[i]+1/n[j]))
if (diff[k] >= hsd[k]) stat[k]<-" *   " 
}

tcomb<-t(comb)
tstat<-cbind(stat)
tr.comp<-cbind(diff)
tHSD<-cbind(hsd)

cat("\nComparison between treatments means\n\nTrt.  Sign   Diff    HSD\n-------------------------\n")

# imprime la significacion entre tratamientos
for(k in 1:nn){
b <-formatC(tr.comp[k],width=6,digits=4)
d <-formatC(hsd[k],width=6,digits=4)
c <-c(tstat[k])
cat(c(tcomb[k,1]),"-",c(tcomb[k,2]),c,b,d,"\n")
}
#comparacion <- data.frame(tcomb,tr.comp,tstat,tLSD)
return(data.frame(tcomb,tr.comp,tstat,tHSD))
# Termina la funcion HSD
}

