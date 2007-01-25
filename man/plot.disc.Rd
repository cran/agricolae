\name{plot.disc}
\alias{plot.disc}
%- plot.disc.
\title{ Discontinuous plotting }
\description{
  This plot processes a discontinuous function.
}
\usage{
plot.disc(x,y,disc,limx=c(min(x), max(x)),limy=c(min(y),max(y)), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ values of the independent variable  }
  \item{y}{ values of the dependent variable  }
  \item{disc}{ points of change of the function in x }
  \item{limx}{limit of x }
  \item{limy}{ limit of y}
  \item{\dots}{ parameters of the function plot }
}

\value{
  \item{x}{ numeric }
  \item{y}{ numeric }
  \item{disc}{ numeric }
  \item{limx}{numeric }
  \item{limy}{ numeric }
  ...
}

\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
# It prepares a data set, in which evalue the function
# Modern theory of probabilities, Parzen 1960

x<-seq(0,12,length=100)
y<-rep(0,length(x))
for(i in 1:length(x)){
if (x[i] <= 0) y[i]<-0
if ((x[i]>0) & (x[i] < 1) ) y[i]<-x[i]/4
if ((x[i]>=1) & (x[i] <= 2) ) y[i]<-1/3
if ((x[i]>2) & (x[i] <= 3) ) y[i]<-x[i]/6
if ((x[i]>3) & (x[i] <= 4) ) y[i]<-1/2
if ((x[i]>4) & (x[i] <= 8) ) y[i]<-x[i]/8
if (x[i] >8) y[i]<-1
}
disc<-c(0,1,2,3,4,8,12)

# execute function.
#startgraph
plot.disc(x,y,disc,type="l",lty=1,col="blue",lwd=2)
#endgraph

}
\keyword{ aplot }% at least one, from doc/KEYWORDS

