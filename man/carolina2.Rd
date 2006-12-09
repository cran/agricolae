\name{carolina2}
\alias{carolina2}
\docType{data}
\title{ Data Carolina II}
\description{
  Data for the analysis of genetic design carolina II.
}
\usage{data(carolina2)}
\format{
  A data frame with 300 observations on the following 9 variables.
  \describe{
    \item{\code{Loc}}{a numeric vector}
    \item{\code{set}}{a numeric vector}
    \item{\code{rep}}{a numeric vector}
    \item{\code{female}}{a numeric vector}
    \item{\code{male}}{a numeric vector}
    \item{\code{plrv}}{a numeric vector}
    \item{\code{yield}}{a numeric vector}
    \item{\code{tuber}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
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
data(carolina2)
str(carolina2)
}
\keyword{datasets}
