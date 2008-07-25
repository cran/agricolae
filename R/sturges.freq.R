`sturges.freq` <-
function (x) 
{
    maximo <- max(x)
    minimo <- min(x)
    amplitud <- maximo - minimo
    n <- length(x)
    k <- round(1 + 3.33 * log10(n),0)
    y<- as.character(x)
    z<-rep(0,n)
    for (i in 1:n) {
      lc<-nchar(y[i])
      nd<-0
      for (j in 1:lc) {
        a <- substr(y[i],j,j)
        if(a!=".")nd=nd+1
        else break
      } 
    z[i]<- lc-nd-1
    }
    d<-max(z)
    if(d < 0) d<-1
    tic <- round(amplitud/k+0.5*10^(-d), d )
    clases <- seq(minimo, maximo, tic)
    nc <- length(clases)
    if (maximo > clases[nc]) 
    clases <- c(clases, clases[nc] + tic)
    lista <- list(maximum = maximo, minimum = minimo, amplitude = amplitud, 
            classes = k, interval = tic, breaks = clases)
    return(lista)
}

