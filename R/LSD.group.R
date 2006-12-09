"LSD.group" <-
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
std.err<-std/sqrt(nn)
t <-length(p)
name<-cbind(row.names(pro))
z<-data.frame(name,p)
Tprob<-qt(1-alpha/2,df)
nr<-unique(n)
nfila<-c("Alpha", "Error Degrees of Freedom", "Error Mean Square",
"Critical Value of t")
nvalor<-c( alpha,  df, MSerror, Tprob)
    cat("\nStudy:", main)
    cat("\n\nLSD t Test for",name.y,"\n")
xtabla<-data.frame("......"=nvalor)
row.names(xtabla)<-nfila
print(xtabla)
if(length(nr)==1) {
valor<-Tprob*sqrt(2*MSerror/nr)
cat("\nLeast Significant Difference",valor)
cat("\nReplication",nr)
}
else {
cat("\nDifferent LSD for each comparison")
cat("\nReplications :\t",n)
}
#
    cat("\n\nTreatments:\n\n")
    nombre <- cbind(row.names(pro))
    resumen <- data.frame(N=n,means=pro,std.error=std.err)
    print(resumen)
    cat("\nMeans with the same letter are not significantly different.")
    
cat("\n\nComparison of treatments\n\n\Groups, Treatments and means\n")
#
output<-order.group(name,p,N=n,MSerror,Tprob,std.err)
return(output)
}

