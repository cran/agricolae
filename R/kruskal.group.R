"kruskal.group" <-
function(y, trt,ties=0,alpha=0.05,main=NULL) {
name.y <- paste(deparse(substitute(y)))
    name <- as.character(unique(trt))
    t <- length(name)
    v <- data.frame(trt, y)
    m <- dim(v)
    v[, 2] <- rank(v[, 2])
    s1 <- by(v[, 2], v[, 1], function(x) sum(x))
    s2 <- as.matrix(s1)
    s3 <- by( v[, 2], v[, 1], function(x) sd(x)/sqrt(length(x)))
    s4 <- as.matrix(s3)
    n <- data.frame(c(by(v, v[, 1], function(x) dim(x))))
    ds <- dim(n)
    N <- sum(n[1, ])
    t <- ds[2]
    rs <- 0
    U <- 0
    for (i in 1:t) {
        rs <- rs + s2[i, 1]^2/n[1, i]
        U <- U + 1/n[1, i]
    }
    if (ties == 0) {
        H <- 12 * rs/(N * (N + 1)) - 3 * (N + 1)
        S <- N * (N + 1)/12
    }
    else {
        S <- (t(v[, 2]) %*% v[, 2] - (N * (N + 1)^2)/4)/(N - 
            1)
        H <- (rs - (N * (N + 1)^2)/4)/S
    }
    cat("\nStudy:",main)
    cat("\nKruskal-Wallis test's\n")
    if (ties == 0) {
        cat("No ties\n")
    }
    else {
        cat("Ties\n")
    }
    cat("\nK-W Value         : ", H)
    cat("\ndegrees of freedom: ", t - 1)
    p.chisq <- 1 - pchisq(H, t - 1)
    p.kw <- dKruskalWallis(H, t, N, U)
    cat("\n\nP.value")
    cat("\nchi.test : ", p.chisq)
    cat("\nK-W.test : ", p.kw, "\n")
    gl <- N - t
    Tprob <- qt(1 - alpha/2, gl)
    rr <- as.numeric(n[1, ])
    nr <- unique(rr)
    means <- s2[, 1]/rr
    std.err<-s4[, 1]
    MSerror <- S * ((N - 1 - H)/(N - t))
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
    cat("\n\n\Groups, Treatments and means of ranks\n")
    name<-row.names(s2)
    output <- order.group(name, means, N=rr, MSerror, Tprob,std.err)

    return(output)
}

