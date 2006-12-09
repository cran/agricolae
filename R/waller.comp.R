"waller.comp" <-
function (y, trt, df, MSerror, Fc, K = 100, main = NULL)
{
    name.y <- paste(deparse(substitute(y)))
    conjunto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    y <- conjunto$y
    trt <- conjunto$trt
    pro <- cbind(by(y, trt, function(x) mean(x)))
    std <- cbind(by(y, trt, function(x) sd(x)))
    nn <- cbind(by(y, trt, function(x) length(x)))
    p <- as.vector(pro[, 1])
    n <- as.vector(nn[, 1])
    t <- length(p)
    std.err<-std/sqrt(nn)
    Tprob <- waller(K,t-1,df,Fc)
nfila<-c("K ratio", "Error Degrees of Freedom", "Error Mean Square","F value",
"Critical Value of Waller")
nvalor<-c( K,  df, MSerror, Fc, Tprob)

    cat("\nStudy:", main)
    cat("\n\nWaller-Duncan K-ratio t Test for",name.y,"\n")
    cat("\nThis test minimizes the Bayes risk under additive")
    cat("\nloss and certain other assumptions.\n")
xtabla<-data.frame("......"=nvalor)
row.names(xtabla)<-nfila
print(xtabla)
cat("\n")
    nombre <- cbind(row.names(pro))
    resumen <- data.frame(treatments=1:t,N=n,means=pro,std.error=std.err)
    print(resumen)
    comb <- combn(t, 2)
    comb <- cbind(comb)
    nn <- ncol(comb)
    diff <- array(0, nn)
    msd <- array(0, nn)
    stat <- array(" ns  ", nn)
    for (k1 in 1:nn) {
        i <- comb[1, k1]
        j <- comb[2, k1]
        diff[k1] <- abs(p[i] - p[j])
        msd[k1] <- Tprob * sqrt(MSerror * (1/n[i] + 1/n[j]))
        if (diff[k1] >= msd[k1])
            stat[k1] <- " *   "
    }
    tcomb <- t(comb)
    tstat <- cbind(stat)
    tr.comp <- cbind(diff)
    MSD <- cbind(msd)
    cat("\nComparison between treatments means\n\nTrt.  Sign   Diff    MSD")
    cat("\n-------------------------\n")
    for (k1 in 1:nn) {
        b <- formatC(tr.comp[k1], width = 6, digits = 4)
        d <- formatC(msd[k1], width = 6, digits = 4)
        c <- c(tstat[k1])
        cat(c(tcomb[k1, 1]), "-", c(tcomb[k1, 2]), c, b, d, "\n")
    }
    return(data.frame(tcomb, tr.comp, tstat, MSD))
}

