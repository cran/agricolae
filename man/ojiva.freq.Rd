\name{ojiva.freq}
\alias{ojiva.freq}
%- ojiva..freq.
\title{ Plotting the ojiva from a histogram }
\description{
  It plots the cumulative relative .frequencies with the intervals of classes
  defined in the histogram.
}
\usage{
ojiva.freq(histogram,type="",xlab="",ylab="",axes="",las=1,...)
}
\arguments{
  \item{histogram}{ object created by the function hist() or graph.freq()}
  \item{type}{ what type of plot should be drawn. See plot()}
  \item{xlab}{ x labels}
  \item{ylab}{ y labels}  
  \item{axes}{ TRUE or FALSE}
  \item{las}{ numeric in {0,1,2,3}; the style of axis labels. see plot()}
  \item{\dots}{ Parameters of the plot() }
}
\value{
  \item{table}{data.frame}
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}}, 
\code{\link{graph.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
data(growth)
h<-graph.freq(growth$height,plot=FALSE)
points<-ojiva.freq(h,col="red",frame=FALSE,
xlab="Height", ylab="Accumulated relative frequency", main="ojiva")
print(points)
}
\keyword{ aplot }% at least one, from doc/KEYWORDS

