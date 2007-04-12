\name{graph.freq}
\alias{graph.freq}
%- graph.freq.
\title{ Frequency table histogram }
\description{
  In many situations it has intervals of class defined with its respective
  frequencies. By means of this function, the picture of frequency is obtained 
  and it is possible to superpose the normal distribution, polygon of frequency, 
  Ojiva and to construct the table of complete frequency.
}
\usage{
graph.freq(breaks, counts, ...)
}

\arguments{
  \item{breaks}{ Class interval }
  \item{counts}{ frequency }
  \item{\dots}{ parameters hist() }
}

\value{
  \item{breaks }{Numeric}
  \item{counts }{Numeric}
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{polygon.freq}}, \code{\link{table.freq}}, \code{\link{stat.freq}},
\code{\link{intervals.freq}}, \code{\link{sturges.freq}}, \code{\link{join.freq}}, 
\code{\link{ojiva.freq}}, \code{\link{normal.freq} } }

\examples{

library(agricolae)
limits <-seq(10,40,5)
frequencies <-c(2,6,8,7,3,4)
#startgraph
h<-graph.freq(limits,frequencies,col="bisque")
polygon.freq(h,col="red")
title( main="Histogram and polygon of frequency",
xlab="Classes", ylab="Frequency")
#endgraph
# Statistics
measures<-stat.freq(h)
print(measures)
# frequency table full
table.freq(h)
#startgraph
# Ojiva
ojiva.freq(h,col="red",type="b",xlab="Variable",ylab="Accumulated relative frequency")
# only frequency polygon
h<-graph.freq(limits,frequencies,border=FALSE,col=NULL)
title( main="Polygon of frequency",
xlab="Variable", ylab="Frecuency")
polygon.freq(h,col="blue")
grid(col="brown")
#endgraph
}
\keyword{ univar }% at least one, from doc/KEYWORDS

