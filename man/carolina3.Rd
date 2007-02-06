\name{carolina3}
\alias{carolina3}
\docType{data}
\title{ Data Carolina III}
\description{
  Data for the analysis of Carolina III genetic design.
}
\usage{data(carolina3)}
\format{
  A data frame with 64 observations on the following 5 variables.
  \describe{
    \item{\code{set}}{a numeric vector}
    \item{\code{male}}{a numeric vector}
    \item{\code{female}}{a numeric vector}
    \item{\code{rep}}{a numeric vector}
    \item{\code{yield}}{a numeric vector}
  }
}

\source{
  Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979.
  table 3. Raw data for B.C. Design-III. pag 207
}
\references{
 Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979.
}
\examples{
library(agricolae)
data(carolina3)
str(carolina3)
}
\keyword{datasets}
