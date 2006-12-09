\name{sweetpotato}
\alias{sweetpotato}
\docType{data}
\title{ Data sweetpotato yield}
\description{
  Yield of sweetpotato under 4 conditions of virus.
}
\usage{data(sweetpotato)}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{virus}}{a factor with levels \code{cc} \code{fc} \code{ff} \code{oo}}
    \item{\code{yield}}{a numeric vector}
  }
}

\source{
  Experimental field.
}
\references{
  International Potato Center. CIP - Lima Peru
}
\examples{
library(agricolae)
data(sweetpotato)
str(sweetpotato)
}
\keyword{datasets}
