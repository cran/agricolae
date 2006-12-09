"cor.mv" <-
function (aa,bb, method = c("pearson", "kendall", "spearman"),alternative = "two.sided") 
{
    method <- match.arg(method)
     if (is.data.frame(aa)) {
        nvarx <- ncol(aa)
        nombrex <- names(aa)
        aa <- as.matrix(aa)
    }
    else {
        nvarx <- 1
        nombrex<- deparse(substitute(aa))
        aa <- as.matrix(aa)
    }
    if (is.data.frame(bb)) {
        nvary <- ncol(bb)
        nombrey <- names(bb)
        bb <- as.matrix(bb)
    }
    else {
        nvary <- 1
        nombrey<- deparse(substitute(bb))
        bb <- as.matrix(bb)
    }
    estimate <- rep(0, nvarx*nvary)
    dim(estimate) <- c(nvarx, nvary)
    dimnames(estimate) <- list(nombrex, nombrey)
    pvalue <- estimate
    nn <- round(estimate, 0)
    for (i in 1:nvarx) {
        for (j in 1:nvary) {
            xx <- cbind(aa[, i], bb[, j])
            yy <- na.omit(xx)
            nn[i, j] <- length(yy[, 1])
            x <- yy[, 1]
            y <- yy[, 2]
            corr <- correl(x, y, method = method, alternative=alternative)
            estimate[i, j] <- corr$rho
            pvalue[i, j] <- corr$pvalue
         }
    }
    names(method) = ""
    estimate <- round(estimate, 2)
    pvalue <- round(pvalue, 4)
    n1<-unique(c(nn))
    if(length(n1)==1)nn<-n1
    cat("\nCorrelation Analysis\n\n")
    lista <- list(method = method, correlation = estimate, pvalue = pvalue, 
        n.obs = nn)
    return(lista)
}

