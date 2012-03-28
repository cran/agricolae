\name{ssp.plot}
\alias{ssp.plot}
%- ssp.plot.
\title{ Split-split-Plot analysis }
\description{
  The variance analysis of a split-split plot design is divided into three parts: 
  the main-plot, subplot and sub-subplot analysis.
}
\usage{
ssp.plot(block, pplot, splot, ssplot, Y)
}

\arguments{
  \item{block}{ replications }
  \item{pplot}{ Factor main plot }
  \item{splot}{ Factor subplot }
  \item{ssplot}{ Factor sub-subplot}
  \item{Y}{ Variable, response }
}
\details{
  The split-split-plot design is an extension of the split-plot design
  to accommodate a third factor: one factor in main-plot, other in subplot 
  and the third factor in sub-subplot
}
\value{
  \item{block}{ vector, numeric or character }
  \item{pplot}{ vector, numeric or character }
  \item{splot}{ vector, numeric or character }
  \item{ssplot}{ vector, nnumeric or character }
  \item{Y}{ vector, numeric }
}
\references{ Statistical procedures for agricultural research.
 Kwanchai A. Gomez, Arturo A. Gomez. Second Edition. 1984.
 }
\author{ Felipe de Mendiburu  }
\seealso{\code{\link{sp.plot}}, \code{\link{strip.plot}}, \code{\link{design.split}}, 
\code{\link{design.strip} } }
\examples{
# Statistical procedures for agricultural research, pag 143
# Grain Yields of Three Rice Varieties Grown under 
#Three Management practices and Five Nitrogen levels; in a
#split-split-plot design with nitrogen as main-plot, 
#management practice as subplot, and variety as sub-subplot 
#factores, with three replications.
library(agricolae)
yield<-c(3.320,3.766,4.660,3.188,3.625,5.232,5.468,5.759,6.215,
4.246,5.255,6.829,3.132,5.389,5.217,3.864,4.311,5.915,4.752,
4.809,5.170,5.788,6.130,7.106,4.842,5.742,5.869,4.375,4.315,
5.389,4.507,4.875,5.400,4.756,5.295,6.046,4.422,5.308,6.318,
4.863,5.345,6.011,4.678,5.896,7.309,6.101,5.096,6.573,5.595,
6.357,7.016,5.442,6.398,6.953,6.209,6.992,7.565,6.860,6.857,
7.254,5.122,4.873,5.495,6.780,5.925,7.442,5.988,6.533,6.914,
6.768,7.856,7.626,6.894,6.974,7.812,4.815,4.166,4.225,5.390,
5.163,4.478,6.509,6.569,7.991,5.779,6.164,7.362,6.573,7.422,
8.950,5.355,7.442,7.018,6.706,8.592,8.480,8.452,8.662,9.112,
8.042,9.080,9.660,9.314,9.224,10.360,5.536,6.462,8.020,6.546,
7.646,9.942,6.698,8.526,9.140,7.414,9.016,8.966,8.508,9.680,
9.896,5.244,5.584,7.642,7.092,7.212,8.714,8.650,8.514,9.320,
6.902,7.778,9.128,8.032,9.294,9.712)
block<-rep(gl(3,15),3)
nitrogen<-rep(c(rep(0,3),rep(50,3),rep(80,3),rep(110,3),rep(140,3)),9)
management<-rep(c("m1","m2","m3"),45)
variety<-gl(3,45)
model<-ssp.plot(block,nitrogen,management,variety,yield)
gla<-model$gl.a; glb<-model$gl.b; glc<-model$gl.c
Ea<-model$Ea; Eb<-model$Eb; Ec<-model$Ec
LSD.test(yield,nitrogen,gla,Ea)
LSD.test(yield,management,glb,Eb)
LSD.test(yield,variety,glc,Ec)
# with aov
A<-data.frame(block, nitrogen,management,variety,yield)
A[,2]<-as.factor(A[,2])
AOV<-aov(yield ~  block + nitrogen*management*variety + Error(block/nitrogen/management),data=A)
summary(AOV)
}
\keyword{ models }% at least one, from doc/KEYWORDS




