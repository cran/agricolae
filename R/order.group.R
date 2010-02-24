`order.group` <-
function(trt,means,N,MSerror,Tprob,std.err,parameter=1, snk=0, DFerror=NULL,alpha=NULL,sdtdif=NULL) {
N<-rep(1/mean(1/N),length(N))
n<-length(means)
z<-data.frame(trt,means,N,std.err)
# ordena tratamientos
w<-z[order(z[,2],decreasing = TRUE), ]
M<-rep("",n)
k<-1
j<-1
k<-1
cambio<-n
cambio1<-0
chequeo=0
M[1]<-letters[k]
while(j<n) {
chequeo<-chequeo+1
if (chequeo > n) break
for(i in j:n) {
nx<-abs(i-j)+1
if (nx==1) nx=2
if(snk ==1 ) Tprob <- qtukey(p=1-alpha,nmeans=nx, df=DFerror)
if(snk ==2 ) Tprob <- qtukey(p=(1-alpha)^(nx-1),nmeans=nx, df=DFerror)
if(is.null(sdtdif))  minimo<-Tprob*sqrt(parameter*MSerror*(1/N[i]+1/N[j]))
if(!is.null(sdtdif)) minimo<-Tprob*sdtdif
s<-abs(w[i,2]-w[j,2])<=minimo
if(s) {
if(lastC(M[i]) != letters[k])M[i]<-paste(M[i],letters[k],sep="")
}
else {
k<-k+1
cambio<-i
cambio1<-0
ja<-j
for(jj in cambio:n) M[jj]<-paste(M[jj]," ",sep="")
M[cambio]<-paste(M[cambio],letters[k],sep="")
for( v in ja:cambio) {
nx<-abs(v-cambio)+1
if(nx == 1)  nx=2
if(snk ==1 ) Tprob <- qtukey(p=1-alpha,nmeans=nx, df=DFerror)
if(snk ==2 ) Tprob <- qtukey(p=(1-alpha)^(nx-1),nmeans=nx, df=DFerror)
if(is.null(sdtdif))  minimo<-Tprob*sqrt(parameter*MSerror*(1/N[i]+1/N[j]))
if(!is.null(sdtdif)) minimo<-Tprob*sdtdif
if(abs(w[v,2]-w[cambio,2])>minimo) {j<-j+1

cambio1<-1
}
else break
}
break
}
}
if (cambio1 ==0 )j<-j+1
}
#-----
w<-data.frame(w,stat=M)
trt<-as.character(w$trt)
means<-as.numeric(w$means)
N<-as.numeric(w$N)
std.err<- as.numeric(w$std.err)
cmax<-max(nchar(trt))
trt<-paste(trt,"                                      ")
trt<-substr(trt,1,cmax)
for(i in 1:n){
cat(M[i],"\t",trt[i],"\t",means[i],"\n")
}
output<-data.frame(trt,means,M,N,std.err)
return(output)
}
