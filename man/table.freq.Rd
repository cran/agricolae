\name{table.freq}
\alias{table.freq}
%- table.freq.
\title{ It displays the table of frequencies }
\description{
  It finds absolute, relative and accumulated the frequency with
  the intervals of class defined of a histogram previously calculated
  by the function "hist" of R.
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

