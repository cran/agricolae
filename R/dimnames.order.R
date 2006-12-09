"dimnames.order" <-
function(x){
a<-x[order(row.names(x)),]
b<-t(a)
d<-b[order(row.names(b)),]
x<-t(d)
return(x)
}

