\name{table.freq}
\alias{table.freq}
%- table.freq.
\title{ frequency Table of a Histogram  }
\description{
  It finds the absolute, relative and accumulated frequencies with the class 
  intervals defined from a previously calculated histogram by the "hist" of R
  function.
}
\usage{
table.freq(object)
}

\arguments{
  \item{object}{ Object by function graph.freq() }
}
\value{
  \item{object }{Object by graph.freq()}
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{stat.freq}}, \code{\link{graph.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}},
\code{\link{ojiva.freq}}, \code{\link{normal.freq} } }          
\examples{
library(agricolae)
data(growth)
attach(growth)
h2<-graph.freq(height,plot=FALSE)
table.freq(h2)
}
\keyword{ distribution }% at least one, from doc/KEYWORDS

