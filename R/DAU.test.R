`DAU.test` <-
function (block, trt, y, method = c("lsd","tukey"),alpha=0.05,group=TRUE)
{
    method<-match.arg(method)
	if(method =="lsd") snk=5
	if(method =="tukey") snk=6
    block.unadj <- as.factor(block)
    trt.adj <- as.factor(trt)
    block.adj <- as.factor(block)
    trt.unadj <- as.factor(trt)
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    modelo1 <- formula(paste(name.y,"~ block.unadj + trt.adj"))
    model1 <- lm(modelo1)
    glerror <- df.residual(model1)
    MSerror <- deviance(model1)/glerror
    modelo2 <- formula(paste(name.y,"~ trt.unadj + block.adj"))
    model2 <- lm(modelo2)
    r <- unique(table(trt.adj))
    b <- nlevels(block.unadj)
    ntr <- nlevels(trt.adj)  # nro trt
    mean.trt<-tapply.stat( y, trt, function(x) mean(x,na.rm=TRUE))
    mean.y<- mean( y, na.rm=TRUE )
    #mean.block<- data.frame(mean.block,ri=mean.block[,2]-mean.y )
    r.trt<-tapply.stat( y, trt, length)
    names(r.trt)[2]="N"
    r.trt<-data.frame(r.trt,control=r.trt[,2]==b,means=mean.trt[,2],mean.adj=mean.trt[,2],block="",std.err=sqrt(MSerror/r.trt[,2]))
    r.trt[,6]<-as.character(r.trt[,6])
    estado <- NULL 
    n<-length(y)
	for (i in 1:n) {
    for (j in 1:ntr) {
    if (trt[i] == r.trt[j,1]) {
    estado[i]<-r.trt[j,3];
    break;
    }
    }}
    
    datos <- data.frame(block, trt, y, control=estado)
    mean.block<-tapply.stat( datos[datos$control,3],datos[datos$control,1], function(x) mean(x,na.rm=TRUE))
    mean.y<- mean( datos[datos$control,3], na.rm=TRUE )
    mean.yy<-mean( datos[,3], na.rm=TRUE )
    mean.block<- data.frame(mean.block,ri=mean.block[,2]-mean.y )
    tc<-sum(datos[,3])^2/n
    # treatment unadjusted
    # comunes vs aumentando
    sc1<-sum(datos[datos$control,3])^2/length(datos[datos$control,3]) + 
    sum(datos[!datos$control,3])^2/length(datos[!datos$control,3]) - tc
    # entre comunes
    unadj <- datos[datos$control,1:3]
    sc2 <- anova(lm(unadj[,3] ~ unadj[,2]))[1,2]
    # entre aumentados
    sc3 <- anova(model2)[1,2] - sc1 - sc2
    # treatment adjusted    
    # Comunes + comunes vs aumentados
    A1 <- anova(model1)
    sc4<- A1[2,2] - sc2 
    A1 <- rbind(A1,A1[3,],A1[3,])
    rownames(A1)[3:5]<-c("Control","Control + control.VS.aug.","Residuals")
    A1[3,1]<-length(r.trt[r.trt$control,3]) - 1
    A1[4,1]<-ntr-length(r.trt[r.trt$control,3])
    A1[3,2]<-sc2
    A1[4,2]<-sc4
    A1[3,3]<-A1[3,2]/A1[3,1]
    A1[4,3]<-A1[4,2]/A1[4,1]
    A1[3,4]<-A1[3,3]/A1[5,3]
    A1[4,4]<-A1[4,3]/A1[5,3]
    A1[3,5]<-1-pf(A1[3,4],A1[3,1],A1[5,1])
    A1[4,5]<-1-pf(A1[4,4],A1[4,1],A1[5,1])
    A1[1,4]<-NA
    A1[1,5]<-NA    
    A2<- anova(model2)
    A2<- rbind(A2,A2[3,],A2[3,],A2[3,])
    rownames(A2)[3:6]<-c("Control","Augmented","Control vs augmented","Residuals")
    A2[3,1]<-length(r.trt[r.trt$control,3]) - 1
    A2[4,1]<-length(r.trt[!r.trt$control,3]) - 1
    A2[5,1]<-1
    A2[3,2]<-sc2
    A2[4,2]<-sc3
    A2[5,2]<-sc1
    A2[3,3]<-A2[3,2]/A2[3,1]
    A2[4,3]<-A2[4,2]/A2[4,1]
    A2[5,3]<-A2[5,2]/A2[5,1]
    A2[3,4]<-A2[3,3]/A2[6,3]
    A2[4,4]<-A2[4,3]/A2[6,3]
    A2[5,4]<-A2[5,3]/A2[6,3]
    A2[3,5]<-1-pf(A2[3,4],A2[3,1],A2[6,1])
    A2[4,5]<-1-pf(A2[4,4],A2[4,1],A2[6,1])        
    A2[5,5]<-1-pf(A2[5,4],A2[5,1],A2[6,1])
    A2[1,4]<-NA
    A2[1,5]<-NA
# Calcula mean adj y ubica el bloque
    for(i in 1:ntr) {
    if(!r.trt[i,3]) {
    for(j in 1:n) {
    if(datos[j,2]== r.trt[i,1] ) {
    marca<-as.character(datos[j,1])
    break
    }
    }
    for(l in 1:b) {
    if(mean.block[l,1]==marca) {
    r.trt[i,6] <- marca
    r.trt[i,5] <- r.trt[i,5]- mean.block[l,3]
    break
    }
    }
    }
    }
# matriz variancia
comunes<-sum(r.trt[,3])
V<-diag(0,ntr)
rownames(V)<-r.trt[,1]
colnames(V)<-r.trt[,1]  
for(i in 1:ntr){
for(j in 1:ntr) {
if(i!=j) {
if (r.trt[i,3]& r.trt[j,3])   V[i,j]<- 2/b
if (!r.trt[i,3]&!r.trt[j,3])  {
    if(r.trt[i,6]==r.trt[j,6] ) V[i,j]<- 2
    else  V[i,j]<- 2*(1+1/comunes)
    }
if (r.trt[i,3] != r.trt[j,3]) V[i,j]<- 1+1/b+1/comunes-1/(b*comunes)

}}}
V<-MSerror*V
#
cat("\nANALYSIS DAU: ", name.y, "\nClass level information\n")
    cat("\nBlock: ", unique(as.character(block)))
    cat("\nTrt  : ", as.character(r.trt[,1]))
    cat("\n\nNumber of observations: ", length(y), "\n")
    cat("\nANOVA, Treatment Adjusted\n")
    print(A1)
    cat("\nANOVA, Block Adjusted\n")
    print(A2)
    cat("\ncoefficient of variation:", round(cv.model(model1), 1),
        "%\n")
    cat(name.y, "Means:", mean.yy, "\n")
    cat("\nCritical Differences (Between)         Std Error Diff.\n")  
cat("\nTwo Control Treatments                       ",sqrt(2*MSerror/b))
cat("\nTwo Augmented Treatments (Same Block)        ",sqrt(2*MSerror))
cat("\nTwo Augmented Treatments(Different Blocks)   ",sqrt(2*MSerror*(1+1/comunes)))
cat("\nA Augmented Treatment and A Control Treatment",sqrt(MSerror*(1+1/b+1/comunes-1/(b*comunes))))
    
    pvalue <- V
    for (i in 1:ntr) {
    for (j in 1:ntr) {
    tc <- abs(r.trt[i,5] - r.trt[j,5])/sqrt(V[i,j])
    if (method == "lsd")
    pvalue[i,j] <- 2 * round(1 - pt(tc, glerror), 6)
    if (method == "tukey")
    pvalue[i,j] <- round(1 - ptukey(tc*sqrt(2), ntr, glerror), 6)
    }
    }

	if(group){
		cat("\n\nMeans with the same letter are not significantly different.")
		cat("\n\nGroups, Treatments and means\n")
		output <- order.group(trt=r.trt[,1], r.trt[,5], r.trt[,2], MSerror=NULL, Tprob=NULL, 
				std.err=r.trt[,7],parameter=1,snk, DFerror=glerror,alpha,sdtdif=1, vartau=V)
		names(output)[2]<-"mean.adj"
	}
    cat("\nComparison between treatments means\n")
    cat("\n<<< to see the objects: comparison and means  >>>\n\n")
    pvalue <- as.dist(pvalue)
    #    N = r, std.err = sqrt(diag(vartau)))
    means<-r.trt[,c(1,4,5,2,6,7)]
    rownames(means)<- means[,1]
    means<-means[,-1]
	if(!group) output=NULL
	invisible(list(means = means,pvalue = pvalue,groups=output))
}
