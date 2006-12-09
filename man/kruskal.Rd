\name{kruskal}
\alias{kruskal}
\docType{data}
\title{ Data kruskal}
\description{
  Data for the non-parametric analysis by kruskal wallis.
}
\usage{data(kruskal)}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{trt}}{a factor with levels \code{T1} \code{T2} \code{T3} \code{T4}}
    \item{\code{obs}}{a numeric vector}
  }
}

\source{
  Book: Practical Nonparametric Statistics.
}
\references{
  Practical Nonparametrics Statistics. W.J. Conover. Third Edition, 1999.
}
\examples{
library(agricolae)
data(kruskal)
str(kruskal)
}
\keyword{datasets}

