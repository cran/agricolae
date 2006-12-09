\name{friedman}
\alias{friedman}
\docType{data}
\title{ Data for friedman test}
\description{
  Twelve homeowners are randomly selected to participate in an experiment with
  a plant nursery. Each homeowner was asked to select four fairly identical
  areas in his yard and to plant four different types of grasses, on in each area.
}
\usage{data(friedman)}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{judge}}{a numeric vector}
    \item{\code{t1}}{a numeric vector}
    \item{\code{t2}}{a numeric vector}
    \item{\code{t3}}{a numeric vector}
    \item{\code{t4}}{a numeric vector}
  }
}

\source{
  Book: Practical Nonparametrics Statistics, pag 372.
}
\references{
  Practical Nonparametrics Statistics. W.J. Conover, 1999
}
\examples{
library(agricolae)
data(friedman)
str(friedman) 
}
\keyword{datasets}
