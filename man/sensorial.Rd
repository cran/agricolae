\name{sensorial}
\alias{sensorial}
\docType{data}
\title{ Data of texture and flavor of native Potato}
\description{
  Data for Durbin Test, evaluated by 15 judges.
}
\usage{data(sensorial)}
\format{
  A data frame with 75 observations on the following 4 variables.
  \describe{
    \item{\code{judge}}{a numeric vector}
    \item{\code{trt}}{a numeric vector}
    \item{\code{texture}}{a numeric vector}
    \item{\code{flavor}}{a numeric vector}
  }
}

\source{
  Experimental data.
}
\references{
  CIP International Potato Center. Lima-Peru.
}
\examples{
library(agricolae)
data(sensorial)
str(sensorial) 
}
\keyword{datasets}
