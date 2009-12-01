\name{AMMI.contour}
\alias{AMMI.contour}
%- AMMI.contour.
\title{ AMMI contour }
\description{
  Draws a polygon or a circumference around the center of the Biplot with a 
  proportional radio at the longest distance of the genotype.
}
\usage{
AMMI.contour(model, distance, shape, ...)
}
\arguments{
  \item{model}{ Object }
  \item{distance}{ Circumference radius >0 and <=1 }
  \item{shape}{ Numerical, relating to the shape of the polygon outline. }
  \item{\dots}{ Parameters corresponding to the R lines function }
}
\details{
First, it is necessary to execute the AMMI function.
It is only valid for the BIPLOT function but not for the TRIPLOT one.
}
\value{
  \item{model}{ output AMMI }
  \item{distance}{ Numeric >0 and <=1 }
  \item{shape}{ Numeric  }
}

\author{ Felipe de Mendiburu }
\note{ 
 Complement graphics AMMI 
}
\seealso{\code{\link{AMMI}} }
\examples{
library(agricolae)
# see AMMI.
data(sinRepAmmi)
attach(sinRepAmmi)
REP <- 3
MSerror <- 93.24224
# Example 1
#startgraph
model<-AMMI(ENV, GEN, REP, YLD, MSerror)
AMMI.contour(model,distance=0.7,shape=8,col="red",lwd=2,lty=5)
#endgraph
# Example 2
#startgraph
for (i in seq(0,0.7,length=15)) {
AMMI.contour(model,distance=i,shape=100,col="green",lwd=2)
}
}
\keyword{ aplot }% __ONLY ONE__ keyword per line
