\name{soil}
\alias{soil}
\docType{data}
\title{ Data of soil analysis for 13 localities}
\description{
  Soil analysis of 13 localities where the CIP makes experiments.
}
\usage{data(soil)}
\format{
  A data frame with 13 observations on the following 23 variables.
  \describe{
    \item{\code{place}}{a factor with levels \code{Chmar} \code{Chz1} \code{Chz2} \code{Cnt1} \code{Cnt2} \code{Cnt3} \code{Hco1} \code{Hco2} \code{Hyo1} \code{Hyo2} \code{Namora} \code{SR1} \code{SR2}}
    \item{\code{pH}}{a numeric vector}
    \item{\code{CE}}{a numeric vector}
    \item{\code{CaCO3}}{a numeric vector}
    \item{\code{MO}}{a numeric vector}
    \item{\code{CIC}}{a numeric vector}
    \item{\code{P}}{a numeric vector}
    \item{\code{K}}{a numeric vector}
    \item{\code{sand}}{a numeric vector}
    \item{\code{slime}}{a numeric vector}
    \item{\code{clay}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{Mg}}{a numeric vector}
    \item{\code{K2}}{a numeric vector}
    \item{\code{Na}}{a numeric vector}
    \item{\code{Al.H}}{a numeric vector}
    \item{\code{K.Mg}}{a numeric vector}
    \item{\code{Ca.Mg}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{Cu}}{a numeric vector}
    \item{\code{Fe}}{a numeric vector}
    \item{\code{Mn}}{a numeric vector}
    \item{\code{Zn}}{a numeric vector}
  }
}

\source{
  Ground samples of Peru where was bacterial wild.
}
\references{
  International Potato Center.
}
\examples{
library(agricolae)
data(soil)
str(soil)
}
\keyword{datasets}
