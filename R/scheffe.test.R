scheffe.test <-
function (y, trt, DFerror, MSerror, Fc, alpha=0.05, group=TRUE,main = NULL,console=FALSE)
{
	name.y <- paste(deparse(substitute(y)))
	name.t <- paste(deparse(substitute(trt)))
	clase<-c("aov","lm")
	if(is.null(main))main<-paste(name.y,"~", name.t)
	if("aov"%in%class(y) | "lm"%in%class(y)){
		if(is.null(main))main<-y$call
		A<-y$model
		DFerror<-df.residual(y)
		MSerror<-deviance(y)/DFerror
		Fc<-anova(y)[trt,4]
		y<-A[,1]
		ipch<-pmatch(trt,names(A))
		nipch<- length(ipch)
		for(i in 1:nipch){
			if (is.na(ipch[i]))
				return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
		}
		name.t<- names(A)[ipch][1]
		trt <- A[, ipch]
		if (nipch > 1){
			trt <- A[, ipch[1]]
			for(i in 2:nipch){
				name.t <- paste(name.t,names(A)[ipch][i],sep=":")
				trt <- paste(trt,A[,ipch[i]],sep=":")
			}}
		name.y <- names(A)[1]
	}
	junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
	Mean<-mean(junto[,1])
	CV<-sqrt(MSerror)*100/Mean	
	medians<-tapply.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
    x <- tapply.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
    medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
	means <- tapply.stat(junto[,1],junto[,2],stat="mean") # change
	sds <-   tapply.stat(junto[,1],junto[,2],stat="sd") #change
	nn <-   tapply.stat(junto[,1],junto[,2],stat="length") # change
	std.err <- sqrt(MSerror)/sqrt(nn[, 2]) # change sds[,2]
	means<-data.frame(means,std=sds[,2],r=nn[,2],se=std.err,medians)
	names(means)[1:2]<-c(name.t,name.y)
#   row.names(means)<-means[,1]
	ntr<-nrow(means)
	Fprob <- qf(1-alpha,ntr-1, DFerror)
	Tprob <- sqrt(Fprob * (ntr - 1))
	nr<-unique(nn[, 2])
	if(console){
	cat("\nStudy:", main)
	cat("\n\nScheffe Test for",name.y,"\n")
	cat("\nMean Square Error  :",MSerror,"\n\n")
	cat(paste(name.t,",",sep="")," means\n\n")
	print(data.frame(row.names = means[,1], means[,-1]))
	cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
	cat("Critical Value of F:", Fprob,"\n")
	}
	scheffe <- Tprob*sqrt(2*MSerror/nr)
	statistics<-data.frame(MSerror=MSerror,Df=DFerror, F=Fprob,Mean=Mean,CV=CV)
	if ( group & length(nr) == 1 & console) cat("\nMinimum Significant Difference:",scheffe,"\n")
if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
if ( length(nr) == 1) statistics<-data.frame(statistics,Scheffe=Tprob,CriticalDifference=scheffe)
		comb <-utils::combn(ntr,2)
		nn<-ncol(comb)
		dif<-rep(0,nn)
		sig<-NULL
		LCL<-dif
		UCL<-dif
		pvalue<-rep(0,nn)
		for (k in 1:nn) {
			i<-comb[1,k]
			j<-comb[2,k]

			dif[k]<-means[i,2]-means[j,2]
			sdtdif<-sqrt(MSerror * (1/means[i,4] + 1/means[j,4]))
			pvalue[k]<- round(1-pf(abs(dif[k])^2/((ntr-1)*sdtdif^2),ntr-1,DFerror),4)
			
			LCL[k] <- dif[k] - Tprob*sdtdif
			UCL[k] <- dif[k] + Tprob*sdtdif
			sig[k]<-" "
			if (pvalue[k] <= 0.001) sig[k]<-"***"
			else  if (pvalue[k] <= 0.01) sig[k]<-"**"
			else  if (pvalue[k] <= 0.05) sig[k]<-"*"
			else  if (pvalue[k] <= 0.1) sig[k]<-"."
		}
		if(!group){
		tr.i <- means[comb[1, ],1]
		tr.j <- means[comb[2, ],1]
		comparison<-data.frame("Difference" = dif, pvalue=pvalue,sig,LCL,UCL)
		rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
        if(console){cat("\nComparison between treatments means\n\n")
		print(comparison)}
		groups=NULL
    }
	if (group) {
	  if(console) {
	  cat("\nMeans with the same letter are not significantly different.\n\n")
 	  }
	  comparison=NULL
	  # Matriz de probabilidades
	  Q<-matrix(1,ncol=ntr,nrow=ntr)
	  p<-pvalue
	  k<-0
	  for(i in 1:(ntr-1)){
	    for(j in (i+1):ntr){
	      k<-k+1
	      Q[i,j]<-p[k]
	      Q[j,i]<-p[k]
	    }
	  }
	  groups <- orderPvalue(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) print(groups)
	}
	parameters<-data.frame(test="Scheffe",name.t=name.t,ntr = ntr,alpha=alpha)
	rownames(parameters)<-" "
	rownames(statistics)<-" "
	rownames(means)<-means[,1]
	means<-means[,-1]
	output<-list(statistics=statistics,parameters=parameters, 
			means=means,comparison=comparison,groups=groups)
	class(output)<-"group"
	invisible(output)
}
