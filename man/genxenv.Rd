\name{genxenv}
\alias{genxenv}
\docType{data}
\title{ Data of yield of potato in environment different}
\description{
  50 genotype and 5 environment.
}
\usage{data(genxenv)}
\format{
  A data frame with 250 observations on the following 3 variables.
  \describe{
    \item{\code{ENV}}{a numeric vector}
    \item{\code{GEN}}{a numeric vector}
    \item{\code{YLD}}{a numeric vector}
  }
}

\source{
  International Potato Center. CIP - Lima Peru.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(genxenv)
str(genxenv)
}
\keyword{datasets}
