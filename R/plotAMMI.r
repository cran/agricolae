plot.AMMI<-function(x,first=1,second=2,third=3,type=1,number=FALSE,gcol=NULL,ecol=NULL,
icol=NULL,angle=25,xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,...)
{
first<-first+2
second<-second+2
third<-third+2
A<-x$biplot
pc<-x$analysis[,1]
n<-length(pc)
namesAxes<-NULL
for(i in 1:n) namesAxes[i]<-paste("PC",i," (",pc[i],")",sep="")
xylabel<-c(names(A)[1:2],namesAxes)
if(is.null(xlab))xlab<-xylabel[first]
if(is.null(ylab))ylab<-xylabel[second]
if(is.null(xlim))xlim<-range(A[,first])
if(is.null(ylim))ylim<-range(A[,second])
if(is.null(gcol))gcol="blue"
if(is.null(ecol))ecol="brown"
if(is.null(icol))icol="green"
gen<-subset(A,A[,1]=="GEN")
env<-subset(A,A[,1]=="ENV")
ngen<-rownames(gen)
if(number)ngen<-1:(nrow(gen))
if(type==1){
plot(A[,first],A[,second],type="p",cex=0,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
text(env[,first],env[,second],rownames(env),col=ecol)
text(gen[,first],gen[,second],ngen,col=gcol)
xmed<-mean(env[,first])
ymed<-mean(env[,second])
arrows(xmed,ymed,env[,first],0.9*env[,second],angle=angle,lty=20)
abline(v=xmed,h=ymed)
}
if(type==2){
lugar <- env[,c(first,second,third)]
clones <- gen[,c(first,second,third)]
maxcp <- max(A[, c(first,second,third)])
mincp <- min(A[, c(first,second,third)])
rango <- maxcp - mincp
clones <- (clones - mincp)/rango
nclon <- nrow(clones)
lugar <- (lugar - mincp)/rango
nlugar <- nrow(lugar)
point1 <- cbind(clones[, 1], clones[, 2], clones[, 3])
point2 <- cbind(lugar[, 1], lugar[, 2], lugar[, 3])
point3 <- cbind(c(0.6, 0.6, 0), c(0, 0.8, 0.8), c(0.6, 0, 0.6))
suppressWarnings(warning(triplot(point1, cex = 0, grid = TRUE,
label = "", center=TRUE,frame=TRUE)))
if (number == TRUE)
suppressWarnings(warning(text(tritrafo(point1), as.character(1:nclon),
adj = c(0.5, 0), col = "blue", cex = 0.8)))
if (number == FALSE)
suppressWarnings(warning(text(tritrafo(point1), rownames(clones),
adj = c(0.5, 0), col = "blue", cex = 0.8)))
suppressWarnings(warning(text(tritrafo(point2), rownames(lugar),
adj = c(0.5, 0), col = "red", cex = 0.8)))
suppressWarnings(warning(text(tritrafo(point3), xylabel[c(first,second,third)],
adj = c(0.5, 0), cex = 1)))
trilines(centerlines(3), lty = 2.5, col = "green", lwd = 2)
for (i in 1:nlugar) {
suppressWarnings(warning(trilines(c(point2[i, 1], 1/3), c(point2[i, 2],
 1/3), c(point2[i, 3], 1/3),col = "red", lty = 1)))
}
}
if(type==3){
plot(A[,first],A[,second],type="p",cex=0,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
text(env[,first],env[,second],rownames(env),col=ecol)
text(gen[,first],gen[,second],ngen,col=gcol)
coords<-as.matrix(gen[,c(first,second)])
tri<-tri2nb(coords)
relative <- graph2nb(soi.graph(tri,coords))
plot(relative,coords,add=TRUE,col=icol,cex=0.1)
}
}