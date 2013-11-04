`PBIB.test` <-
		function (block, trt, replication, y, k, method=c("REML","ML","VC"), 
		test = c("lsd", "tukey"), alpha = 0.05, group = TRUE,console=FALSE)
{

	test <- match.arg(test)
	if (test == "lsd")
		snk = 3
	if (test == "tukey")
		snk = 4
	method <- match.arg(method)
	if(method=="REML") nMethod<-"Residual (restricted) maximum likelihood"
	if(method=="ML") nMethod<-"Maximum likelihood"
	if(method=="VC") nMethod<-"Variances component model"
	name.y <- paste(deparse(substitute(y)))
	name.r <- paste(deparse(substitute(replication)))
	name.b <- paste(deparse(substitute(block)))
	name.trt <- paste(deparse(substitute(trt)))
	block.adj <- as.factor(block)
	trt.adj <- as.factor(trt)
	replication <- as.factor(replication)
	mean.trt <- as.matrix(by(y, trt, function(x) mean(x,na.rm=TRUE)))
	mi <- as.matrix(by(y, trt, function(x) min(x,na.rm=TRUE)))
	ma <- as.matrix(by(y, trt, function(x) max(x,na.rm=TRUE)))
	n.rep <- as.matrix(by(y, trt, function(x) length(na.omit(x))))
	sds<- as.matrix(by(y, trt, function(x) sd(x,na.rm=TRUE)))
	std.err<-sds/sqrt(n.rep)
	indice <- rownames(mean.trt)
	
	ntr <- nlevels(trt.adj)
	r <- nlevels(replication)
	s <- ntr/k
	obs <- length(na.omit(y))
	# Use function lm #
	if (method=="VC" & obs != r*ntr ) {
		if(console)cat("\nWarning.. incomplete repetition. Please you use method REML or ML\n")
		return()
	}
	modelo <- formula(paste(name.y, "~ replication + trt.adj+ block.adj%in%replication"))
	model <- lm(modelo)
	glerror <- df.residual(model)
	ANOVA<-anova(model)
	rownames(ANOVA)[2]<-name.trt
	CV<- cv.model(model)
	Mean<-mean(y,na.rm=TRUE)
	if (method == "VC") {
		rownames(ANOVA)<- c(name.r,paste(name.trt,".unadj",sep=""),paste(name.b,"/",name.r,sep=""),"Residual")
	}
# Use function lmer #
	if (method == "REML" | method == "ML") {
		trt.adj <- as.factor(trt)

		if (method == "REML"){
			modlmer <- lmer(y ~  0+(1 | replication) + trt.adj + (1 | replication/block.adj), REML=TRUE)
			model <- lmer(y ~ (1 | replication) + trt.adj + (1 | replication/block.adj), REML=TRUE)
		}
		if (method == "ML"){
			modlmer <- lmer(y ~  0+(1 | replication) + trt.adj + (1 | replication/block.adj), REML=FALSE)
			model <- lmer(y ~ (1 | replication) + trt.adj + (1 | replication/block.adj), REML=FALSE)
		}
		Afm<-anova(model)
		VarRand<- as.numeric(VarCorr(model))
		VarRand[2]<-VarRand[2]+VarRand[3]
		CMerror<-sigma(model)^2
		VarRand[3]<-CMerror
		VarRand<-data.frame(VarRand)
		names(VarRand)<-"Variance"
		rownames(VarRand)<-c(paste(name.b,":",name.r,sep=""),name.r,"Residual")
		ANOVA<-ANOVA[c(2,4),]
		ANOVA[1,1:4]<-Afm
		CMerror<-sigma(model)^2
		ANOVA[2,3]<-CMerror
		ANOVA[,2]<-ANOVA[,1]*ANOVA[,3]
		ANOVA[2,4:5]<-NA
		ANOVA[1,5]<-1-pf(ANOVA[1,4],ANOVA[1,1],ANOVA[2,1])
	}
#
	b <- s * r
	glt <- ntr - 1
	if (method == "VC") {
		SCt<- anova(model)[2, 2]
		Ee <- deviance(model)/glerror
		Eb <- anova(model)[3, 3]
		###  means ###
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
		phi <- r * (Eb - Ee)/((r - 1) * Ee)
		lambda <- 1/(r * k * (1/phi + 1) - k)
		W <- t(N) %*% N - k * Ib - g * kronecker((Jr - Ir), Js)
		inversa <- ginv(Ib - lambda * W)
		tauIntra <- t(X) %*% y/r - lambda * N %*% inversa %*% c0
		vartau <- (Ee/r) * (Iv + lambda * N %*% inversa %*% t(N))
		dvar <- sqrt(diag(vartau))
	}

	# use function lmer #
	if (method == "REML" | method == "ML") {
		tauIntra<-fixef(modlmer)
		vartau <- vcov(modlmer)
		DIA<-as.matrix(vartau)
		dvar<-sqrt(diag(DIA))
	}
# -------------------
	vardif <- matrix(0, ntr, ntr)
	for (i in 1:(ntr - 1)) {
		for (j in (i + 1):ntr) {
			vardif[i, j] <- vartau[i, i] + vartau[j, j] - 2 *
					vartau[i, j]
			vardif[j, i] <- vardif[i, j]
		}
	}
	media<-mean(y, na.rm = TRUE)
	if(console){
	cat("\nANALYSIS PBIB: ", name.y, "\n\nClass level information\n")
	cat(name.b,":",b,"\n")
	cat(name.trt,":", ntr)
	cat("\n\nNumber of observations: ", length(y), "\n\n")
	cat("Estimation Method: ",nMethod,"\n\n")
	}
	if (method == "REML" | method == "ML") {
		Fstat<-data.frame(c(deviance(model),AIC(model),BIC(model)))
		names(Fstat)<-"Fit Statistics"
		rownames(Fstat)<-c("-2 Res Log Likelihood","AIC","BIC")		
		if(console){
		cat("Parameter Estimates\n")
		print(VarRand)
		cat("\n")
		print(Fstat)
		cat("\n")
		}
		CV<- sqrt(CMerror)*100/media
	}
	design<-data.frame("."=c(ntr,k,b/r,r))
	rownames(design)<-c(name.trt,paste(name.b,"size"),paste(name.b,"/",name.r,sep=""),name.r)
	E <- (ntr - 1) * (r - 1)/((ntr - 1) * (r - 1) + r * (s-1))
	if(console){	
	print(ANOVA)
	cat("\ncoefficient of variation:", round(CV,1), "%\n")
	cat(name.y, "Means:", media, "\n")
	cat("\nParameters PBIB\n")
	print(design)
	cat("\nEfficiency factor", E, "\n")
	cat("\nComparison test", test, "\n")
	}
	parameters<-data.frame(treatments=ntr,blockSize=k,blocks=s,r=r)
	statistics<-data.frame(Efficiency=E,Mean=Mean,CV=CV)
	rownames(parameters)<-" "
	rownames(statistics)<-" "
	comb <- combn(ntr, 2)
	nn <- ncol(comb)
	dif <- rep(0, nn)
	stdt <- rep(0, nn)
	pvalue <- rep(0, nn)
	for (k in 1:nn) {
		i <- comb[1, k]
		j <- comb[2, k]
#		if (tauIntra[i] < tauIntra[j]) {
#			comb[1, k] <- j
#			comb[2, k] <- i
#		}
        dif[k]<- tauIntra[i] - tauIntra[j]
		stdt[k] <- sqrt(vartau[i, i] + vartau[j, j]- 2 * vartau[i,j])
		tc <- abs(dif[k])/stdt[k]
		if (test == "lsd")
			pvalue[k] <- 2 * round(1 - pt(tc, glerror),6)
		if (test == "tukey")
			pvalue[k] <- round(1 - ptukey(tc, ntr, glerror),6)
	}
	tr.i <- comb[1, ]
	tr.j <- comb[2, ]
	if (group) {
		if(console){cat("\nMeans with the same letter are not significantly different.")
		cat("\n\nGroups, Treatments and means\n")}
		groups <- order.group(trt = 1:ntr, tauIntra, n.rep, MSerror = NULL,
				Tprob = NULL, std.err = dvar, parameter = 1,
				snk, DFerror = glerror, alpha, sdtdif = 1, vartau,console=console)
		names(groups)[2] <- "mean.adj"
		rownames(groups)<- groups$trt
		indices<-as.numeric(as.character(groups$trt))
		groups$trt<-indice[indices]
		names(groups)[1] <- name.trt
		groups<-groups[,1:3]
	}
	if(console){cat("\nComparison between treatments means and its name\n")
	cat("\n<<< to see the objects: means, comparison and groups. >>>\n\n")}
	comparison <- data.frame(Difference = dif, stderr = stdt,
			pvalue = pvalue)
	rownames(comparison) <- paste(tr.i, tr.j, sep = " - ")
	means <- data.frame(means = mean.trt,trt = 1:ntr,  mean.adj = as.numeric(tauIntra),
			SE = dvar, r = n.rep, std.err=std.err,Min.=mi,Max.=ma)
	names(means)[1]<-name.y
	if (!group) groups = NULL
	output<-list(method=nMethod,parameters=parameters,statistics=statistics ,
			comparison = comparison, means = means, groups = groups, vartau = vartau)
			
	invisible(output)
}
