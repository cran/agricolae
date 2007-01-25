\name{AMMI.contour}
\alias{AMMI.contour}
%- AMMI.contour.
\title{ AMMI contour }
\description{
  It makes a polygon or a circumference around the center part of the Biplot with a 
  proportional radio at the longest distance of the genotype.
}
\usage{
AMMI.contour(model, distance, shape, ...)
}
\arguments{
  \item{model}{ Object }
  \item{distance}{ Radio de la circunferencia >0 and <=1 }
  \item{shape}{ Numerico, relacionado con la forma del contorno del poligono. }
  \item{\dots}{ Parametros correspondiente a la fiuncion lines de R }
}
\details{
Es necesario primero ejecutar la funcion AMMI.
Solo es valido para la funcion BIPLOT no para TRIPLOT 
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
model<-AMMI(ENV, GEN, REP, YLD, MSerror,xlim=c(-8,6),ylim=c(-6,6))
AMMI.contour(model,distance=0.7,shape=8,col="red",lwd=2,lty=5)
#endgraph
# Example 2
#startgraph
for (i in seq(0,0.7,length=15)) {
AMMI.contour(model,distance=i,shape=100,col="green",lwd=2)
}
}
\keyword{ aplot }% __ONLY ONE__ keyword per line
