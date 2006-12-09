\name{trees}
\alias{trees}
\docType{data}
\title{ Data species trees. Pucallpa}
\description{
  species: Guaba, Laurel, Roble and  Terminalia.
}
\usage{data(trees)}
\format{
  A data frame with 24 observations on the following 3 variables.
  \describe{
    \item{\code{place}}{a numeric vector}
    \item{\code{species}}{a factor with levels \code{GUABA} \code{LAUREL} \code{ROBLE} \code{TERMINALIA}}
    \item{\code{diameter}}{a numeric vector}
  }
}

\source{
  CATIE Centro Agronomico Tropical.
}
\references{
  ICRAF-Peru
}
\examples{
library(agricolae)
data(trees)
str(trees)
}
\keyword{datasets}
