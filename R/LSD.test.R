`LSD.test` <-
function (y, trt, DFerror, MSerror, alpha = 0.05, p.adj = c("none",
    "holm", "hochberg", "bonferroni", "BH", "BY", "fdr"), group = TRUE,
    main = NULL)
{
    p.adj <- match.arg(p.adj)
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[, 1], junto[, 2], stat="mean") #change
    sds <- tapply.stat(junto[, 1], junto[, 2], stat="sd")     #change
    nn <- tapply.stat(junto[, 1], junto[, 2], stat="length")  #change
    std.err <- sds[, 2]/sqrt(nn[, 2])
    Tprob <- qt(1 - alpha/2, DFerror)
    LCI <- means[,2]-Tprob*std.err
    UCI <- means[,2]+Tprob*std.err
    means <- data.frame(means, std.err, replication = nn[, 2],
    LCI, UCI)
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
    nfila <- c("Alpha", "Error Degrees of Freedom", "Error Mean Square",
        "Critical Value of t")
    nvalor <- c(alpha, DFerror, MSerror, Tprob)
    cat("\nStudy:", main)
    cat("\n\nLSD t Test for", name.y, "\n")
    if (p.adj != "none")
        cat("P value adjustment method:", p.adj, "\n")
    xtabla <- data.frame(...... = nvalor)
    row.names(xtabla) <- nfila
    print(xtabla)
    cat("\nTreatment Means and Individual (1-alpha)*100% CI\n")
    print(data.frame(row.names = NULL, means))
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
        pvalue <- rep(0, nn)
        for (k in 1:nn) {
            i <- comb[1, k]
            j <- comb[2, k]
            dif[k] <- abs(means[i, 2] - means[j, 2])
            sdtdif <- sqrt(MSerror * (1/means[i, 4] + 1/means[j,
                4]))
            pvalue[k] <- 2 * (1 - pt(dif[k]/sdtdif, DFerror))
            if (p.adj != "none")
                pvalue[k] <- p.adjust(pvalue[k], n = nk, p.adj)
                pvalue[k] <- round(pvalue[k],4)
        }
        tr.i <- comb[1, ]
        tr.j <- comb[2, ]
        cat("\nComparison between treatments means\n\n")
        print(data.frame(row.names = NULL, tr.i, tr.j, diff = dif,
            pvalue = pvalue))
        output <- data.frame(trt = means[, 1], means = means[,
            2], M = "", N = means[, 4], std.err ,LCI,UCI)
    }
    return(output)
    }

