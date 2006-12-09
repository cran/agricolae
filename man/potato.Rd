\name{potato}
\alias{potato}
\docType{data}
\title{ Data of cutting}
\description{
  Study of yield of two varieties of potato in the experimental station
  of the CIP.
}
\usage{data(potato)}
\format{
  A data frame with 18 observations on the following 4 variables.
  \describe{
    \item{\code{date}}{a numeric vector}
    \item{\code{variety}}{a factor with levels \code{Canchan} \code{Unica}}
    \item{\code{harvest}}{a numeric vector}
    \item{\code{cutting}}{a numeric vector}
  }
}

\source{
  Experimental data.
}
\references{
  International Potato Center
}
\examples{
library(agricolae)
data(potato)
str(potato)
}
\keyword{datasets}
