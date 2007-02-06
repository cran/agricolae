\name{decimals}
\alias{decimals}
%- decimals.
\title{ Finding the largest decimal number from the elements of a vector or matrix  }
\description{
  In many situations it is required to have a larger decimal number than a set
  of values expressed in climbing, vector or matrix.
}
\usage{
decimals(x)
}

\arguments{
  \item{x}{ data  }
}
\value{
  \item{x }{constant, vector or matrix}
}
\author{ Felipe de Mendiburu }
\seealso{\code{\link{sturges.freq} } }

\examples{
library(agricolae)
x<-c(10.43,40.8,3.1415,879.28)
decimals(x)
# result is 4

}
\keyword{ manip }% at least one, from doc/KEYWORDS

