\name{sturges.freq}
\alias{sturges.freq}
%- sturges.freq.
\title{ Intervals of class for a histogram, the rule of Sturges }
\description{
  Number classes: k = 1 + 3.32 log10 (N)
}
\usage{
sturges.freq(x)
}
\arguments{
  \item{x}{ vector }
}

\value{
  \item{x }{Numeric}
}
\author{ Felipe de mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{graph.freq}}, \code{\link{join.freq}},
\code{\link{ojiva.freq}}, \code{\link{normal.freq} } }

\examples{
library(agricolae)
data(natives)
attach(natives)
classes<-sturges.freq(size)
# information of the classes
classes
intervals <- classes$classes
#startgraph
# Histogram with the established classes
h1<-hist(size,breaks=intervals,freq=TRUE, col="yellow",axes=FALSE,
    xlim=c(0,0.12),main="",xlab="",ylab="")
axis(1,intervals,las=2)
axis(2,seq(0,400,50),las=2)
title(main="Histogram of frequency\nSize of the tubercule of the Oca",
xlab="Size of the oca", ylab="Frequency")
#endgraph

}
\keyword{ manip }% at least one, from doc/KEYWORDS

