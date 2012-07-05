\name{intervals.freq}
\alias{intervals.freq}
%- intervals.freq.
\title{ Class intervals }
\description{
  List class intervals.
}
\usage{
intervals.freq(breaks)
}

\arguments{
  \item{breaks}{ class intervals }
}

\value{
  \item{breaks}{ vector numeric }
}
\author{ Felipe de Mendiburu }
\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{graph.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}}, 
\code{\link{ojiva.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
# example 1
data(growth)
attach(growth)
h2<-hist(height,plot=FALSE)
intervals.freq(h2$breaks)
# example 2
x<-seq(10,40,5)
y<-c(2,6,8,7,3,4)
intervals.freq(x)
}
\keyword{ univar }% at least one, from doc/KEYWORDS

