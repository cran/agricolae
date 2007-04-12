\name{wilt}
\alias{wilt}
\docType{data}
\title{ Data of Bacterial Wilt (AUDPC) and soil}
\description{
  The data correspond to the progress of the disease of the Bacterial Wilt 
  in CIP lands. 
}
\usage{data(wilt)}
\format{
  A data frame with 13 observations on the following 5 variables.
  \describe{
    \item{\code{audpc}}{a numeric vector}
    \item{\code{CE}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{K2}}{a numeric vector}
    \item{\code{Cu}}{a numeric vector}
  }
}

\source{
  Experimental data.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(wilt)
str(wilt) 
}
\keyword{datasets}
