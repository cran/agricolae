\name{plot.AMMI}
\alias{plot.AMMI}
%- plot.AMMI.
\title{ PLOT AMMI }
\description{
  Biplot AMMI.
}
\usage{
\method{plot}{AMMI}(x,first=1,second=2,third=3,number=FALSE,gcol=NULL,ecol=NULL,
angle=25,lwd=1.8,length=0.1,xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,...)
}
\arguments{
  \item{x}{ object AMMI }
  \item{first}{ position axis x, 0=Y-dependent, 1=PC1, 2=PC2, 3=PC3}
  \item{second}{ position axis y,0=Y-dependent, 1=PC1, 2=PC2, 3=PC3}
  \item{third}{ position axis z,0=Y-dependent, 1=PC1, 2=PC2, 3=PC3}
  \item{number}{ TRUE or FALSE names or number genotypes}
  \item{gcol}{ genotype color}
  \item{ecol}{ environment color }
  \item{angle}{ angle from the shaft of the arrow to the edge of the arrow head }
  \item{lwd}{ parameter line width in function arrow}
  \item{length}{ parameter length in function arrow}
  \item{xlab}{ x labels}
  \item{ylab}{ y labels} 
  \item{xlim}{ x limites}
  \item{ylim}{ y limites}
  \item{\dots}{ other parameters of plot}
}

\details{
Produce graphs biplot.
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{AMMI}} }

\examples{
library(agricolae)
data(plrv)
model<- with(plrv,AMMI(Locality, Genotype, Rep, Yield))
# biplot PC2 vs PC1
plot(model)
## plot PC1 vs Yield
plot(model,0,1,gcol="blue",ecol="green")
}
\keyword{ aplot }% at least one, from doc/KEYWORDS
