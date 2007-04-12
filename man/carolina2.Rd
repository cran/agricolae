\name{carolina2}
\alias{carolina2}
\docType{data}
\title{ Data Carolina II}
\description{
  Data for the analysis of Carolina II Genetic design.
  Both paternal and maternal half-sibs are produced in this design.
  From an F2 population, n1 males and n2 females are randomly selected
  and each male is crossed to each of the females. Thus n1 x n2 progenies
  are produced whitch are analysed in a suitably laid experiment.
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
