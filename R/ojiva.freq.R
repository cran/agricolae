"ojiva.freq" <-
function(histogram,...)
{
    xx <- histogram$mids
    yy <- histogram$counts
    zz <- histogram$breaks
    y1 <- sum(yy)
    x <- length(xx)
    zz<-c(zz,2*zz[x+1]-zz[x])
    z <- rep(0, x+1)
    for (i in 1:x) {
        z[i+1] <- z[i] + yy[i]/y1
    }
    z<-c(z,1)
    plot(zz,z,...)
    grid(col="black")
    table<-data.frame(class.sup=zz,F.rel=z)
    return(table)
}

