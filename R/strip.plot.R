`strip.plot` <-
function (BLOCKS, COL, ROW, Y)
{
name.y <- paste(deparse(substitute(Y)))
name.col <- paste(deparse(substitute(COL)))
name.row <- paste(deparse(substitute(ROW)))
name.block <- paste(deparse(substitute(BLOCKS)))
cat("\nANALYSIS STRIP PLOT: ", name.y, "\nClass level information\n")
COL <- as.factor(COL)
ROW <- as.factor(ROW)
BLOCKS <- as.factor(BLOCKS)
nrep <- length(unique(BLOCKS))
nCOL <- length(unique(COL))
nROW <- length(unique(ROW))
cat("\n", name.col,  "\t   (COL): ",unique(as.character(COL)))
cat("\n", name.row,  "\t   (ROW): ",unique(as.character(ROW)))
cat("\n", name.block,"\t(BLOCKS): ",unique(as.character(BLOCKS)))
#cat("\nBLOCKS: ", unique(BLOCKS))
cat("\n\nNumber of observations: ", length(Y), "\n\n")
model <-aov(Y~BLOCKS+COL+  BLOCKS:COL + ROW + BLOCKS:ROW+ COL:ROW)
cat("model Y:", name.y, " ~ BLOCKS + COL  + BLOCKS:COL + ROW + BLOCKS:ROW+ Error(BLOCKS:COL+BLOCKS:ROW)+ COL:ROW\n\n")
mm <- anova(model)
nn <- mm[3, ]
nn1<- row.names(mm)[3]
nn2<- row.names(mm)[4]
row.names(mm)[4] <- " "
mm[3, ] <- mm[4, ]
mm[4, ] <- nn
row.names(mm)[3] <- nn2
row.names(mm)[4] <- nn1
mm[2, 4] <- mm[2, 3]/mm[3, 3]
mm[2, 5] <- 1 - pf(mm[2, 4], mm[2, 1], mm[3, 1])
mm[4, 4] <- mm[4, 3]/mm[5, 3]
mm[4, 5] <- 1 - pf(mm[4, 4], mm[4, 1], mm[5, 1])
print(mm)
DFE <- df.residual(model)
MSE <- deviance(model)/DFE
medy <- mean(Y)
cat("\nCoeff var", "\tMean", name.y, "\n")
cat(sqrt(MSE) * 100/medy, "\t", medy, "\n")
gl.a<-mm[3,1]; Ea<-mm[3,3]
gl.b<-mm[5,1]; Eb<-mm[5,3]
gl.c<-mm[7,1]; Ec<-mm[7,3]

output<-list(anva=mm, model=model,gl.a=gl.a,gl.b=gl.b,gl.c=gl.c,Ea=Ea,Eb=Eb,Ec=Ec)
return(output)
}

