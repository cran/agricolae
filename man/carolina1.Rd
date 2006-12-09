\name{carolina1}
\alias{carolina1}
\docType{data}
\title{ Data Carolina I}
\description{
 Data for the analysis of genetic design carolina I.
}
\usage{data(carolina1)}
\format{
  A data frame with 72 observations on the following 6 variables.
  \describe{
    \item{\code{set}}{a numeric vector}
    \item{\code{male}}{a numeric vector}
    \item{\code{female}}{a numeric vector}
    \item{\code{progenie}}{a numeric vector}
    \item{\code{rep}}{a numeric vector}
    \item{\code{yield}}{a numeric vector}
  }
}

\source{
  Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979.
}
\references{
 Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979.
}
\examples{
library(agricolae)
data(carolina1)
str(carolina1)
}
\keyword{datasets}
