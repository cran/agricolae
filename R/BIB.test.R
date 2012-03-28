`BIB.test` <-
function (block, trt, y, method = c("lsd","tukey","duncan","waller","snk"), alpha = 0.05, group = TRUE)
{
    method<-match.arg(method)
    block.unadj <- as.factor(block)
    trt.adj <- as.factor(trt)
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    modelo <- formula(paste(name.y,"~ block.unadj + trt.adj"))
    model <- lm(modelo)
    DFerror <- df.residual(model)
    MSerror <- deviance(model)/DFerror
    k <- unique(table(block.unadj))
    r <- unique(table(trt.adj))
    b <- nlevels(block.unadj)
    ntr <- nlevels(trt.adj)
    lambda <- r * (k - 1)/(ntr - 1)
    datos <- data.frame(block, trt, y)
    tabla <- by(datos[,3], datos[,1:2], function(x) mean(x,na.rm=TRUE))
    tabla <-as.data.frame(tabla[,])
    AA <- !is.na(tabla)
    BB <- tapply(y, block.unadj, function(x) sum(x, na.rm=TRUE))
    B <- BB %*% AA
    Y <- tapply(y, trt.adj, function(x) sum(x, na.rm=TRUE))
    Q <- Y - as.numeric(B)/k
    SStrt.adj <- sum(Q^2) * k/(lambda * ntr)
    MStrt.adj <- SStrt.adj/(ntr - 1)
    sdtdif <- sqrt(2 * k * MSerror/(lambda * ntr))
    Fvalue <- MStrt.adj/MSerror
    mean.adj <- mean(y,na.rm=TRUE) + Q * k/(lambda * ntr)
    StdError.adj <- sqrt(MSerror * (1 + k * r * (ntr - 1)/(lambda *
        ntr))/(r * ntr))
    cat("\nANALYSIS BIB: ", name.y, "\nClass level information\n")
    cat("\nBlock: ", unique(as.character(block)))
    cat("\nTrt  : ", unique(as.character(trt)))
    cat("\n\nNumber of observations: ", length(y), "\n\n")
    print(anova(model))
    cat("\ncoefficient of variation:", round(cv.model(model), 1),
        "%\n")
    cat(name.y, "Means:", mean(y,na.rm=TRUE), "\n\n")
    cat(paste(name.t,",",sep="")," statistics\n\n")
    nameTrt<-row.names(Y)
    print(data.frame(row.names = nameTrt, means=Y/r,mean.adj, StdError.adj))
    parameter <- k/(lambda * ntr)
    snk<-0
    if (method == "lsd") {
        Tprob <- qt(1 - alpha/2, DFerror)
        cat("\nLSD test")
        cat("\nStd.diff   :", sdtdif)
        cat("\nAlpha      :", alpha)
        cat("\nLSD        :", Tprob * sdtdif)
    }
    if (method == "tukey") {
        Tprob <- qtukey(1 - alpha, ntr, DFerror)
        sdtdif<-sdtdif/sqrt(2)
        cat("\nTukey")
        cat("\nAlpha      :", alpha)
        cat("\nStd.err    :", sdtdif)
        cat("\nHSD        :", Tprob * sdtdif)
        parameter <- parameter/2
    }
    if (method == "waller") {
        K <- 650 - 16000 * alpha + 1e+05 * alpha^2
        Tprob <- waller(K, ntr - 1, DFerror, Fvalue)
        cat("\nWaller-Duncan K-ratio")
        cat("\nThis test minimizes the Bayes risk under additive")
        cat("\nloss and certain other assumptions.\n")
        cat("\nk Ratio    : ", K)
        cat("\nMSD        :", Tprob * sdtdif)
    }
    if (method == "snk") {
        snk<-1
        sdtdif<-sdtdif/sqrt(2)
        cat("\nStudent Newman Keuls")
        cat("\nAlpha     :", alpha)
        cat("\nStd.err   :", sdtdif)
        Tprob <- qtukey(1-alpha,2:ntr, DFerror)
        SNK <- Tprob * sdtdif
        names(SNK)<-2:ntr
        cat("\nCritical Range\n")
        print(SNK)
        }
      if (method == "duncan") {
        snk<-2
        sdtdif<-sdtdif/sqrt(2)
        cat("\nDuncan's new multiple range test")
        cat("\nAlpha     :", alpha)
        cat("\nStd.err   :", sdtdif)
        Tprob <- qtukey((1-alpha)^(1:(ntr-1)),2:ntr, DFerror)
        duncan <- Tprob * sdtdif
        names(duncan)<-2:ntr
        cat("\n\nCritical Range\n")
        print(duncan)
    }
    E <- lambda * ntr/(r * k)
    cat("\nParameters BIB")
    cat("\nLambda     :", lambda)
    cat("\ntreatmeans :", ntr)
    cat("\nBlock size :", k)
    cat("\nBlocks     :", b)
    cat("\nReplication:", r, "\n")
    cat("\nEfficiency factor", E, "\n\n<<< Book >>>\n")
    if (group) {
        cat("\nMeans with the same letter are not significantly different.")
        cat("\n\nComparison of treatments\n\nGroups, Treatments and means\n")
        if (snk==0)
        output <- order.group(names(mean.adj), as.numeric(mean.adj),
            rep(1, ntr), MSerror, Tprob, std.err = StdError.adj,
            parameter,sdtdif=sdtdif)
        else output <- order.group(names(mean.adj), as.numeric(mean.adj),
            rep(1, ntr), MSerror, Tprob, std.err = StdError.adj,
            parameter, snk=snk,DFerror,alpha,sdtdif=sdtdif)
        output[, 4] <- r
    }
    if (!group) {
        Omeans<-order(mean.adj,decreasing = TRUE)
        Ordindex<-order(Omeans)
        comb <- combn(ntr, 2)
        nn <- ncol(comb)
        dif <- rep(0, nn)
        sig <- rep(" ",nn)
        pvalue <- dif
        odif<-dif
        for (k in 1:nn) {
            i <- comb[1, k]
            j <- comb[2, k]
            if (mean.adj[i] < mean.adj[j]){
            comb[1, k]<-j
            comb[2, k]<-i
            }
            dif[k] <- abs(mean.adj[i] - mean.adj[j])
            if (method == "lsd")
                pvalue[k] <- 2 * round(1 - pt(dif[k]/sdtdif,
                  DFerror), 6)
            if (method == "tukey")
                pvalue[k] <- round(1 - ptukey(dif[k] /sdtdif,
                  ntr, DFerror), 6)
            if (method == "snk"){
                odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
                pvalue[k] <- round(1 - ptukey(dif[k] /sdtdif,
                odif[k], DFerror), 6)
                }
        	if (method == "duncan"){
	        	nx<-abs(i-j)+1
	        	odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
				pvalue[k]<- round((1-ptukey(dif[k]/sdtdif,odif[k],DFerror))^1/(odif[k]-1),6)
        		}
        sig[k]<-" "
		if (pvalue[k] <= 0.001) sig[k]<-"***"
		else  if (pvalue[k] <= 0.01) sig[k]<-"**"
		else  if (pvalue[k] <= 0.05) sig[k]<-"*"
		else  if (pvalue[k] <= 0.1) sig[k]<-"."
        }
        if (method == "waller")
            significant = dif > Tprob * sdtdif
        tr.i <- nameTrt[comb[1, ]]
        tr.j <- nameTrt[comb[2, ]]
        cat("\nComparison between treatments means\n")
        if (method == "waller") output<-data.frame("Difference" = dif, significant)
        else  output<-data.frame("Difference" = dif, pvalue=pvalue,sig)
        rownames(output)<-paste(tr.i,tr.j,sep=" - ")
        print(output)
        output <- data.frame(trt = names(mean.adj), means = as.numeric(mean.adj),
            M = "", N = r, std.err = StdError.adj)
    }
    invisible(output)
}
