\name{huasahuasi}
\alias{huasahuasi}
\docType{data}
\title{ data Yield in Huasahuasi }
\description{
  Potato growing in huasahuasi. Peru
}
\usage{data(huasahuasi)}
\format{
  A data frame with 45 observations on the following 9 variables.
  \describe{
    \item{\code{Block}}{a factor with levels \code{I} \code{II} \code{III}}
    \item{\code{Treat}}{a factor with levels \code{40mm} \code{7dias} \code{SinAplic}}
    \item{\code{Clon}}{a factor with levels \code{386209.1} \code{387164.4} \code{Cruza148} \code{Musuq} \code{Yungay}}
    \item{\code{Comercial}}{a numeric vector}
    \item{\code{y1da}}{a numeric vector}
    \item{\code{y2da}}{a numeric vector}
    \item{\code{y3ra}}{a numeric vector}
    \item{\code{yield}}{a numeric vector}
    \item{\code{AUDPC}}{a numeric vector}
  }
}

\source{
  International Potato Center. CIP - Lima Peru.
}
\references{
  International Potato Center. CIP - Lima Peru.
}
\examples{
library(agricolae)
data(huasahuasi)
str(huasahuasi)
}
\keyword{datasets}
