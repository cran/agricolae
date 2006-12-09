\name{polygon.freq}
\alias{polygon.freq}
%- polygon.freq.
\title{ The polygon of frequency on the histogram }
\description{
  The polygon is constructed single or on histogram,
  is necessary to execute the function previously hist.
}
\usage{
polygon.freq(histogram, ...)
}

\arguments{
  \item{histogram}{ Object constructed by the function hist }
  \item{\dots}{ Other parameters of the function hist }
}

\value{
  \item{histogram }{Object}
}
\author{ Felipe de Mendiburu Delgado }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}},
\code{\link{graph.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
data(growth)
attach(growth)
#startgraph
h1<-hist(height,border=FALSE,xlim=c(6,14))
polygon.freq(h1,col="red")
#endgraph
#startgraph
h2<-hist(height,col="yellow",xlim=c(6,14))
polygon.freq(h2,col="red")
#endgraph
}
\keyword{ aplot }% at least one, from doc/KEYWORDS



