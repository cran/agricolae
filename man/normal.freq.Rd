\name{normal.freq}
\alias{normal.freq}
%- normal.freq.
\title{ Normal curve on the histogram }
\description{
  A normal distribution graph elaborated from the histogram previously
  constructed. The average and variance are obtained from the data grouped
  in the histogram.
}
\usage{
normal.freq(histogram, probability = FALSE, ...)
}

\arguments{
  \item{histogram}{ object constructed by the function hist}
  \item{probability}{ when histogram is by density }
  \item{\dots}{ Other parameters of the function hist }
}

\value{
  \item{Histogram }{object}
  \item{probability }{logic False or True}
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}},
\code{\link{ojiva.freq}}, \code{\link{graph.freq} } }

\examples{
library(agricolae)
data(growth)
attach(growth)
#startgraph
h1<-hist(height,col="green",xlim=c(8,14))
normal.freq(h1,col="blue")
#endgraph
#startgraph
h2<-hist(height,col="yellow",xlim=c(8,14),probability=TRUE)
normal.freq(h2,probability=TRUE)
#endgraph
}
\keyword{ aplot }% at least one, from doc/KEYWORDS

