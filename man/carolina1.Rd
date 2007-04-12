\name{carolina1}
\alias{carolina1}
\docType{data}
\title{ Data Carolina I}
\description{
 Data for the analysis of Carolina I genetic design.
 In this design F2 or any advanced generation maintained by random mating, 
 produced from cross between two pure-lines, is taken as base population.
 From the population an individual is randomly selected and used as a male.
 A set of 4 randomly selected plans are used as females and are mated to the
 above male. Thus a set of 4 full-sib families are produced. This is denoted
 as a male group. Similarly, a large number of male groups are produced. 
 No female is used for any second mating. four male groups (16 female groups)
 from a set. 
  
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
