`LSD.test` <-
function (y, trt, DFerror, MSerror, alpha = 0.05, p.adj = c("none",
    "holm", "hochberg", "bonferroni", "BH", "BY", "fdr"), group = TRUE,
    main = NULL)
{
    p.adj <- match.arg(p.adj)
    clase<-c("aov","lm")
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if("aov"%in%class(y) | "lm"%in%class(y)){
    A<-y$model
    DFerror<-df.residual(y)
    MSerror<-deviance(y)/DFerror
    y<-A[,1]
    ipch<-pmatch(trt,names(A))
    if( is.na(ipch)) return(cat("Name: ",trt,"\n",names(A)[-1],"\n"))
    name.t <-names(A)[ipch]
    trt<-A[,ipch]
    name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[, 1], junto[, 2], stat="mean") #change
    sds <- tapply.stat(junto[, 1], junto[, 2], stat="sd")     #change
    nn <- tapply.stat(junto[, 1], junto[, 2], stat="length")  #change
    std.err <- sds[, 2]/sqrt(nn[, 2])
    Tprob <- qt(1 - alpha/2, DFerror)
    LCL <- means[,2]-Tprob*std.err
    UCL <- means[,2]+Tprob*std.err
    means <- data.frame(means, std.err, replication = nn[, 2],
    LCL, UCL)
    names(means)[1:2] <- c(name.t, name.y)
    #row.names(means) <- means[, 1]
    ntr <- nrow(means)
    nk <- choose(ntr, 2)
    if (p.adj != "none")
        {
        a <- 1e-06
        b <- 1
        for (i in 1:100) {
            x <- (b + a)/2
            d <- p.adjust(x, n = nk, p.adj) - alpha
            fa <- p.adjust(a, n = nk, p.adj) - alpha
            if (d * fa < 0)
                b <- x
            if (d * fa > 0)
                a <- x
        }
        Tprob <- qt(1 - x/2, DFerror)
    }
    nr <- unique(nn[, 2])
    cat("\nStudy:", main)
    cat("\n\nLSD t Test for", name.y, "\n")
    if (p.adj != "none")
        cat("P value adjustment method:", p.adj, "\n")
    cat("\nMean Square Error: ",MSerror,"\n\n")
    cat(paste(name.t,",",sep="")," means and individual (",(1-alpha)*100,"%) CI\n\n")
    print(data.frame(row.names = means[,1], means[,-1]))
    cat("\nalpha:",alpha,"; Df Error:",DFerror)
    cat("\nCritical Value of t:", Tprob,"\n")
    if (group) {
        if (length(nr) == 1) {
            LSD <- Tprob * sqrt(2 * MSerror/nr)
            cat("\nLeast Significant Difference", LSD)
        }
        else {
            nr1 <- 1/mean(1/nn[, 2])
            LSD1 <- Tprob * sqrt(2 * MSerror/nr1)
            cat("\nLeast Significant Difference", LSD1)
            cat("\nHarmonic Mean of Cell Sizes ", nr1)
            }
        cat("\nMeans with the same letter are not significantly different.")
        cat("\n\nGroups, Treatments and means\n")
        output <- order.group(means[, 1], means[, 2], means[,
            4], MSerror, Tprob, means[, 3])
        w<-order(means[,2],decreasing = TRUE)
        output <- data.frame(output,LCI=means[w,5],UCI=means[w,6])
    }
    if (!group) {
        comb <- combn(ntr, 2)
        nn <- ncol(comb)
        dif <- rep(0, nn)
        LCL1<-dif
		UCL1<-dif
        sig<-NULL
        pvalue <- rep(0, nn)
        for (k in 1:nn) {
            i <- comb[1, k]
            j <- comb[2, k]
            if (means[i, 2] < means[j, 2]){
            comb[1, k]<-j
            comb[2, k]<-i
            }
            dif[k] <- abs(means[i, 2] - means[j, 2])
            sdtdif <- sqrt(MSerror * (1/means[i, 4] + 1/means[j,
                4]))
            pvalue[k] <- 2 * (1 - pt(dif[k]/sdtdif, DFerror))
            if (p.adj != "none")
                pvalue[k] <- p.adjust(pvalue[k], n = nk, p.adj)
                pvalue[k] <- round(pvalue[k],6)
        LCL1[k] <- dif[k] - Tprob*sdtdif
		UCL1[k] <- dif[k] + Tprob*sdtdif
        sig[k]<-" "
        if (pvalue[k] <= 0.001) sig[k]<-"***"
        else  if (pvalue[k] <= 0.01) sig[k]<-"**"
        else  if (pvalue[k] <= 0.05) sig[k]<-"*"
        else  if (pvalue[k] <= 0.1) sig[k]<-"."
        }
        tr.i <- means[comb[1, ],1]
        tr.j <- means[comb[2, ],1]
        output<-data.frame("Difference" = dif, pvalue = pvalue,sig,LCL=LCL1,UCL=UCL1)
        rownames(output)<-paste(tr.i,tr.j,sep=" - ")
        cat("\nComparison between treatments means\n\n")
        print(output)
        output <- data.frame(trt = means[, 1], means = means[,
            2], M = "", N = means[, 4], std.err ,LCL,UCL)
    }
    invisible(output)
    }
