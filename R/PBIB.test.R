`PBIB.test` <-
function (block, trt, replication, y, k, method = "lsd", alpha = 0.05)
{
    block.adj <- as.factor(block)
    trt.unadj <- as.factor(trt)
    replication <- as.factor(replication)
    name.y <- paste(deparse(substitute(y)))
    modelo <- formula(paste(name.y,"~ replication + trt.unadj+ block.adj%in%replication"))
    model <- lm(modelo)
    SCt <- anova(model)[2, 2]
    ntr <- nlevels(trt.unadj)
    r <- nlevels(replication)
    s <- ntr/k
    obs <- ntr * r
    b <- s * r
    glt <- ntr - 1
    glerror <- df.residual(model)
    Ee <- deviance(model)/glerror
    Eb <- anova(model)[3, 3]
    mean.trt <- tapply.stat(y, trt, mean)[, 2]   # change
    X <- rep(0, obs * ntr)
    dim(X) <- c(obs, ntr)
    for (i in 1:obs) {
        tr <- trt[i]
        X[i, tr] <- 1
    }
    R <- rep(0, obs * r)
    dim(R) <- c(obs, r)
    for (i in 1:obs) {
        rp <- replication[i]
        R[i, rp] <- 1
    }
    Z <- rep(0, obs * b)
    dim(Z) <- c(obs, b)
    for (i in 1:obs) {
        rb <- block[i]
        Z[i, rb] <- 1
    }
    N <- t(X) %*% Z
    In <- diag(1, obs)
    c0 <- t(Z) %*% (In - (1/r) * X %*% t(X)) %*% y
    Js <- diag(s)
    Ir <- diag(r)
    Jr <- matrix(1, r, r)
    Js <- matrix(1, s, s)
    Ib <- diag(b)
    Iv <- diag(ntr)
    q <- k - floor(k/s) * s
    if (q <= s/2)
        g <- floor(k/s)
    if (q > s/2)
        g <- floor(k/s) + 1
    phi <- r*(Eb-Ee)/( (r-1)*Ee)
    lambda <- 1/(r*k*(1/phi+1)-k)
#   lambda <- 1/(r * k - k) only 1/phi = zero
    W <- t(N) %*% N - k * Ib - g * kronecker((Jr - Ir), Js)
####################
#    c1 <- W%*%c0
#    for (ic in 1:100) {
#    c1 <- W%*%c1
#    }
#    c2 <- W%*%c1
#    print(c0); print(c1); print(c2)
########################
    inversa <- ginv(Ib - lambda * W)
    tauIntra <- t(X) %*% y/r - lambda * N %*% inversa %*% c0
    vartau <- (Ee/r) * (Iv + lambda * N %*% inversa %*% t(N))
#   vartau <- (Ee/r) * (Iv + lambda * N %*%t(N)+lambda^2 * N%*%W%*%t(N))
    vardif <- matrix(0, ntr, ntr)
    for (i in 1:(ntr - 1)) {
        for (j in (i + 1):ntr) {
            vardif[i, j] <- vartau[i, i] + vartau[j, j] - 2 *
                vartau[i, j]
            vardif[j, i] <- vardif[i, j]
        }
    }
    cat("\nANALYSIS PBIB: ", name.y, "\nClass level information\n")
    cat("\nBlocks: ", b)
    cat("\nTrts  : ", ntr)
    cat("\n\nNumber of observations: ", length(y), "\n\n")
    print(anova(model))
    cat("\ncoefficient of variation:", round(cv.model(model), 1),
        "%\n")
    cat(name.y, "Means:", mean(y,na.rm=TRUE), "\n") 
    cat("\nTreatments\n")
    cat("\nParameters PBIB")
    cat("\ntreatmeans :", ntr)
    cat("\nBlock size :", k)
    cat("\nBlocks/rep :", b/r)
    cat("\nReplication:", r, "\n")
    E <- (ntr - 1) * (r - 1)/((ntr - 1) * (r - 1) + r * (s -
        1))
    cat("\nEfficiency factor", E, "\n")
    comb <- combn(ntr, 2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    stdt <- rep(0, nn)
    pvalue <- rep(0, nn)
    for (k in 1:nn) {
        i <- comb[1, k]
        j <- comb[2, k]
        dif[k] <- abs(tauIntra[i] - tauIntra[j])
        stdt[k] <- sqrt(vartau[i, i] + vartau[j, j] - 2 * vartau[i,j])
        tc <- dif[k]/stdt[k]
        if (method == "lsd")
            pvalue[k] <- 2 * round(1 - pt(tc, glerror), 4)
        if (method == "tukey")
            pvalue[k] <- round(1 - ptukey(tc, ntr, glerror),
                4)
    }
    tr.i <- comb[1, ]
    tr.j <- comb[2, ]
    cat("\nComparison between treatments means\n")
    cat("\n<<< to see the objects: comparison and means  >>>\n\n")
    comparison <- data.frame(row.names = NULL, tr.i, tr.j, diff = dif, stderr=stdt,
        pvalue = pvalue)
    means <- data.frame(trt = 1:ntr, means = mean.trt, mean.adj = as.numeric(tauIntra),
        N = r, std.err = sqrt(diag(vartau)))
    return(list(comparison = comparison, means = means,vartau=vartau))
}