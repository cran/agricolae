\name{homog1}
\alias{homog1}
\docType{data}
\title{ Data of fríjol }
\description{
  Data of fríjol under 4 technologies for study of homogenidad of regression.
  Yield of Frijol in kg/ha. In clean and dry grain

  Tecnnologies:
  20-40-20 kg/ha.  N. P2O5 and K2O + 2 t/ha of gallinaza.
  40-80-40 kg/ha.  N. P2O5 and K2O + 2 t/ha of gallinaza.
  60-120-60 kg/ha. N. P2O5 and K2O + 2 t/ha of gallinaza.
  40-80-40 kg/ha.  N. P2O5 and K2O + 4 t/ha of gallinaza.
}
\usage{data(homog1)}
\format{
  A data frame with 84 observations on the following 3 variables.
  \describe{
    \item{\code{technology}}{a factor with levels \code{a} \code{b} \code{c} \code{d}}
    \item{\code{production}}{a numeric vector}
    \item{\code{index}}{a numeric vector}
  }
}

\references{
Oriente antioqueño (1972) (ICA.- Orlando Martinez W.)
Colombia.
}
\examples{
library(agricolae)
data(homog1)
str(homog1)
}
\keyword{datasets}

