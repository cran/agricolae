\name{montecarlo}
\alias{montecarlo}

\title{ Random generation by Montecarlo }
\description{Random generation form data, use function density and parameters
  
}
\usage{
montecarlo(data, k, ...)
}

\arguments{
  \item{data}{ vector  or object(hist, graph.freq) }
  \item{k}{ number of simulations }
  \item{\dots}{ Other parameters of the function density, only if data is vector }  
}

\value{
  Generates random.
}

\author{ Felipe de Mendiburu }
\seealso{\code{\link{density}} }
\examples{
library(agricolae)
r<-rnorm(50, 10,2)
montecarlo(r, k=100, kernel="epanechnikov")
# other example
h<-hist(r,plot=FALSE)
montecarlo(h, k=100)
# other example
breaks<-c(0, 150, 200, 250, 300)
counts<-c(10, 20, 40, 30)
h<-graph.freq(x=breaks,counts=counts,plot=FALSE)
montecarlo(h, k=100)
}
\keyword{ manip }
