\name{table.freq}
\alias{table.freq}
%- table.freq.
\title{ Frequency Table of a Histogram  }
\description{
  It finds the absolute, relative and accumulated frequencies with the class 
  intervals defined from a previously calculated histogram by the "hist" of R
  function.
}
\usage{
table.freq(histogram)
}

\arguments{
  \item{histogram}{ Object by function hist() }
}
\value{
  \item{histogram }{Object by hist()}
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{stat.freq}}, \code{\link{graph.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}},
\code{\link{ojiva.freq}}, \code{\link{normal.freq} } }          
\examples{
library(agricolae)
data(growth)
attach(growth)
h2<-hist(height,plot=FALSE)
table.freq(h2)
}
\keyword{ distribution }% at least one, from doc/KEYWORDS

