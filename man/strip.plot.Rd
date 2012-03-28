\name{strip.plot}
\alias{strip.plot}
%- Strip.plot.
\title{ Strip-Plot analysis }
\description{
  The variance analysis of a strip-plot design is divided into three parts: 
  the horizontal-factor analysis, the vertical-factor analysis, and the 
  interaction analysis.
}
\usage{
strip.plot(BLOCK, COL, ROW, Y)
}

\arguments{
  \item{BLOCK}{ replications }
  \item{COL}{ Factor column }
  \item{ROW}{ Factor row }
  \item{Y}{ Variable, response }
}
\details{
  The strip-plot design is specifically suited for a two-factor experiment
  in which the desired precision for measuring the interaction effects
  between the two factors is higher than that for measuring the main efect
  two factors
}
\value{
  \item{BLOCK}{ vector, numeric }
  \item{COL}{ vector, numeric }
  \item{ROW}{ vector, numeric }
  \item{Y}{ vector, numeric }
}
\references{ Statistical procedures for agricultural research.
 Kwanchai A. Gomez, Arturo A. Gomez. Second Edition. 1984.
 }
\author{ Felipe de Mendiburu  }
\seealso{\code{\link{ssp.plot}}, \code{\link{sp.plot}}, \code{\link{design.split}}, 
\code{\link{design.strip} } }
\examples{
# Yield
library(agricolae)
data(huasahuasi)
attach(huasahuasi)
YIELD<-huasahuasi$YIELD
attach(YIELD)
market <- y1da + y2da
non_market <- y3da
yield <- market + non_market
model<-strip.plot(block, clon, trt, yield)
comparison<-LSD.test(yield,clon,model$gl.a,model$Ea)
comparison<-LSD.test(yield,trt,model$gl.b,model$Eb)
# simple effects
A<-model$data
a<-nlevels(A$clon)
b<-nlevels(A$trt)
r<-nlevels(A$block)
Ea<-model$Ea; Eb<-model$Ec; Ec<-model$Ec;
gla<-model$gl.a; glb<-model$gl.b; glc<-model$gl.c;
B <-tapply.stat(A[,4],A[,2:3],mean)
std<-tapply.stat(A[,4],A[,2:3],function(x) sd(x)/sqrt(length(x)))
B<-data.frame(B[,1:2],yield=B[,3],std=std[,3])
cmab<-(b-1)*Ec + Ea
cmba<-(a-1)*Ec + Eb
# order.group
# Tukey
ta <- qtukey(0.95,a,gla)
tb <- qtukey(0.95,b,glb)
tc <- qtukey(0.95,a*b,glc)
tab<- ((b-1)*Ec*tc + Ea*ta)/cmab
tba<- ((a-1)*Ec*tc + Eb*tb)/cmba
# Comparison of clones by treatment
groups<-by(B,B[,2], function(x)
        order.group(x$clon,x$yield,N=b*r,cmab,tab, std.err=x$std,parameter=0.5))
groups
# Comparison of treatments by clon
groups<-by(B,B[,1], function(x)
        order.group(x$trt,x$yield,N=a*r,cmba,tba, std.err=x$std,parameter=0.5))
groups
# LSD t-student
ta <- qt(0.975,gla)
tb <- qt(0.975,glb)
tc <- qt(0.975,glc)
tab<- ((b-1)*Ec*tc + Ea*ta)/cmab
tba<- ((a-1)*Ec*tc + Eb*tb)/cmba
# Comparison of clones by treatment
groups<-by(B,B[,2], function(x)
        order.group(x$clon,x$yield,N=b*r,cmab,tab, std.err=x$std))
groups
# Comparison of treatments by clon
groups<-by(B,B[,1], function(x)
        order.group(x$trt,x$yield,N=a*r,cmba,tba, std.err=x$std))
groups
}
\keyword{ models }% at least one, from doc/KEYWORDS
