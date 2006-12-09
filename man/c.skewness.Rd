\name{c.skewness}
\alias{c.skewness}
%- c.skewness.
\title{ Computes the coefficient of skewness }
\description{
  Returns the skewness of a distribution. Similar to SAS.
}
\usage{
c.skewness(x)
}

\arguments{
  \item{x}{a numeric vector}
}
\details{

n = length(x)

$\frac{n}{(n-1)(n-2)}\sum_{i}\left(\frac{x_i - mean(x)}{sd(x)}\right)^3$

}
\value{

  \item{x }{The skewness of x}
}

\seealso{\code{\link{c.kurtosis} } }

\examples{
library(agricolae)
x<-c(3,4,5,2,3,4,NA,5,6,4,7)
c.skewness(x)
# value is 0,3595431, is slightly asimetrica (positive) to the right
}
\keyword{ univar }% at least one, from doc/KEYWORDS

