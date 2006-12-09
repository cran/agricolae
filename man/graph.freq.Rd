\name{graph.freq}
\alias{graph.freq}
%- graph.freq.
\title{ Histogram of a frequency table }
\description{
  In many situations it is had intervals of class defined with its
  respective frequencies. By means of this function, the picture of
  frequency is obtained and is possible to superpose the normal distribution,
  poligono of frequency, Ojiva and to construct the table of complete frequency
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
x<-seq(10,40,5)
y<-c(2,6,8,7,3,4)
#startgraph
h<-graph.freq(x,y,col="bisque")
polygon.freq(h,col="red")
title( main="Histogram and polygon of frequency",
xlab="Variable X", ylab="Frequency")
#endgraph
# Statistics
measures<-stat.freq(h)
print(measures)
# frequency table full
table.freq(h)
#startgraph
# Ojiva
ojiva.freq(h,col="red",type="b")
# only frequency polygon
h<-graph.freq(x,y,border=FALSE,col=NULL)
title( main="Polygon of frequency",
xlab="Variable X", ylab="Frecuency")
polygon.freq(h,col="blue")
grid(col="brown")
#endgraph
}
\keyword{ univar }% at least one, from doc/KEYWORDS

