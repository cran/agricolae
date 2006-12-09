\name{ralstonia}
\alias{ralstonia}
\docType{data}
\title{ Data of population bacterial wild: AUDPC}
\description{
  Data of population of the bacterial wild evaluated to 2, 15, 29, 43, 58
  and 73 days.
}
\usage{data(ralstonia)}
\format{
  A data frame with 13 observations on the following 6 variables.
  \describe{
    \item{\code{V1}}{a numeric vector}
    \item{\code{V2}}{a numeric vector}
    \item{\code{V3}}{a numeric vector}
    \item{\code{V4}}{a numeric vector}
    \item{\code{V5}}{a numeric vector}
    \item{\code{V6}}{a numeric vector}
  }
}

\source{
  Experimental field.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(ralstonia)
str(ralstonia)
}
\keyword{datasets}
