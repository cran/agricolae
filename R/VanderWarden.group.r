"waerden.group" <-
function(y, trt,alpha=0.05,main=NULL) {
name.y <- paste(deparse(substitute(y)))
    name <- as.character(unique(trt))
    k <- length(name)
    v <- data.frame(trt, y)
    m <- dim(v)
    N<- length(v[,2])
    v[, 2] <- qnorm(round(rank(v[, 2]) /(N+1),3))
    s1 <- by(v[, 2], v[, 1], function(x) mean(x))
    s2 <- as.matrix(s1)
    s3 <- by( v[, 2], v[, 1], function(x) sd(x)/sqrt(length(x)))
    s4 <- as.matrix(s3)
    S <- sum(v[, 2]^2)/(N-1)
    n <- data.frame(c(by(v, v[, 1], function(x) dim(x))))

    ds <- dim(n)
    #N <- sum(n[1, ])
    t <- ds[2]
    #rs <- 0
    T1 <- 0
    for (i in 1:k) {
        T1 <- T1 + s2[i, 1]^2*n[1, i]
    }
    T1<-T1/S
    cat("\nStudy:",main)
    cat("\nVan der Waerden (Normal Scores) test's\n")
    cat("\nValue             : ", T1)
    cat("\ndegrees of freedom: ", k - 1)
    p.chisq <- 1 - pchisq(T1, k - 1)
    cat("\n\nP.value")
    cat("\nchisq.test : ", p.chisq, "\n")
    gl <- N - t
    Tprob <- qt(1 - alpha/2, gl)
    rr <- as.numeric(n[1, ])
    nr <- unique(rr)
    means <- s2[, 1]
    std.err<-s4[, 1]
    MSerror <- S * ((N - 1 - T1)/(N - t))
    cat("\nComparison of treatments")
    #...............
    cat("\nSignificant\n")
    cat("level: ",alpha,"\n")
    cat("\nt-Student   : ", Tprob)
    if (length(nr) == 1) {
        valor <- Tprob * sqrt(2 * MSerror/nr)
        cat("\nReplications: ", nr)
        cat("\nLSD value   : ", valor)
    }
    else {
        cat("\nReplications:    ", rr)
        cat("\nMinimum difference changes for each comparison")
    }
    cat("\n\n\Groups, Treatments and means of the normal score\n")
    name<-row.names(s2)
    output <- order.group(name, means, N=rr, MSerror, Tprob,std.err)

    return(output)
}

