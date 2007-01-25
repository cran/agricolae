\name{join.freq}
\alias{join.freq}
%- join.freq.
\title{ Join class for histogram }
\description{
  In many situations it is required to join classes because of the low frequency
  in the intervals. In this process, it is required to join the intervals and
  ad the frequencies of them.
 }
\usage{
join.freq(breaks, join)
}

\arguments{
  \item{breaks}{ Class intervals }
  \item{join}{ vector }
}
}
\value{
  \item{breaks }{vector numeric}
  \item{join }{numeric}

}
\author{ Felipe de Mendiburu }
 
\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{graph.freq}},
\code{\link{ojiva.freq}}, \code{\link{normal.freq} } }
 
\examples{
library(agricolae)
data(natives)
attach(natives)
class<-sturges.freq(size)
# information of the classes
class
intervals <- class$classes
# list classes
intervals.freq(intervals)
# Table frecuency
h1<-hist(size,breaks=intervals,plot=FALSE)
table.freq(h1)
# Join classes 9,10,11 and 12 with little frequency.
inter<-join.freq(intervals,9:12) # with c(9,10,11,12) or 9:12
# new table
h2<-hist(size,breaks=inter,freq=TRUE,xlim=c(0,0.12),col="bisque")
table.freq(h2)

}
\keyword{ univar }% at least one, from doc/KEYWORDS

