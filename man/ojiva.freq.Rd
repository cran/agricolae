\name{ojiva.freq}
\alias{ojiva.freq}
%- ojiva..freq.
\title{ Plotting the ojiva from a histogram }
\description{
  It plots the cumulative relative .frequencies with the intervals of classes
  defined in the histogram.
}
\usage{
ojiva.freq(histogram, ...)
}
\arguments{
  \item{histogram}{ object created by the function hist() or graph.freq()}
  \item{\dots}{ Parameters of the hist() }
}
\value{
  \item{histogram }{Object}
  ...
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}}, 
\code{\link{graph.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
data(growth)
attach(growth)

#startgraph
h<-graph.freq(height,plot=FALSE)
detach(growth)
points<-ojiva.freq(h,type="l",col="red",frame=FALSE,
xlab="Limit of class", ylab="Accumulated relative frequency", main="ojiva")
grid(col="black")
#endgraph
print(points)
}
\keyword{ aplot }% at least one, from doc/KEYWORDS

