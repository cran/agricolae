"waller.group" <-
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
    std.err<-std/sqrt(nn)
    t <- length(p)
    name <- cbind(row.names(pro))
    z <- data.frame(name, p)
    Tprob <- waller(K,t-1,df,Fc)
    nr <- unique(n)
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

    if (length(nr) == 1) {
        valor <- Tprob * sqrt(2 * MSerror/nr)
        cat("\nMinimum Significant Difference", valor)
        cat("\nReplication", nr)
    }
    else {
        cat("\nDifferent MSD for each comparison")
        cat("\nReplications", n)
    }
    cat("\n\nTreatments:\n\n")
    nombre <- cbind(row.names(pro))
    resumen <- data.frame(N=n,means=pro,std.error=std.err)
    print(resumen)
    cat("\nMeans with the same letter are not significantly different.")
    cat("\n\nComparison of treatments\n\nGroups, Treatments and means\n")
    output <- order.group(name, p, N=n, MSerror, Tprob,std.err)
    return(output)
}

