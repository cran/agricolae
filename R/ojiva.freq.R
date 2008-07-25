`ojiva.freq` <-
function(histogram,...)
{
    xx <- histogram$mids
    yy <- histogram$counts
    zz <- histogram$breaks
    y1 <- sum(yy)
    nx <- length(xx)
    x <-c(zz,2*zz[nx+1]-zz[nx])
    y <- rep(0, nx+1)
    for (i in 1:nx) {
        y[i+1] <- y[i] + yy[i]/y1
    }
    probability<-c(y,1)
    plot(x,probability,...)
    table<-data.frame(x=zz,probability=y)
    return(table)
}

