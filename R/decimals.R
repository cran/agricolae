"decimals" <-
function(x) {
k<-length(x)
m<-k
n<-0
digits <- 0
a<-1
while (m > 0) {
m<-0
for (i in 1:k) {
accion <- (a*x[i] == trunc(a*x[i]))

if( ! accion ) {
m<-1
stop
}
}
if (m > 0) {
a<-10*a
digits<- digits +1
}
}
return (digits)
}

