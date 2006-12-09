\name{wild}
\alias{wild}
\docType{data}
\title{ Data of Bacterial wild (AUDPC) and soil}
\description{
  Relation of the disease and some components of the ground.
}
\usage{data(wild)}
\format{
  A data frame with 13 observations on the following 5 variables.
  \describe{
    \item{\code{audpc}}{a numeric vector}
    \item{\code{CE}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{K2}}{a numeric vector}
    \item{\code{Cu}}{a numeric vector}
  }
}

\source{
  Experimental data.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(wild)
str(wild) 
}
\keyword{datasets}
