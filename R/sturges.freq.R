"sturges.freq" <-
function(x){
maximo<-max(x)
minimo<-min(x)
amplitud<-maximo-minimo
n<-length(x)
k<-ceiling(1+3.32*log10(n))
d<-decimals(x)
tic<-round(amplitud/k,d+1)
clases<-seq(minimo,maximo,tic)
nc<-length(clases)
if(maximo > clases[nc]) clases<-c(clases,clases[nc]+tic)
k<-length(clases)-1
lista<-list(maximum=maximo,minimum=minimo,amplitude=amplitud,K.classes=k,size.interval=tic,classes=clases)
return(lista)
}

