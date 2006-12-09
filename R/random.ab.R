"random.ab" <-
function(p,q){ 
t<-rep(0,3*p*q)
dim(t)<-c(p*q,3)
k<-0
for(i in 1:p){
for(j in 1:q){
k<-k+1
t[k,1]<-i
t[k,2]<-j
t[k,3]<-runif(1)
}
}
return(t[order(t[,3]),-3])}

