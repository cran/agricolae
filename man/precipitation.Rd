\name{precipitation}
\alias{precipitation}
\docType{data}
\title{ Data of precipitation}
\description{
  Data of precipitation for stability.
}
\usage{data(precipitation)}

\format{
  A data frame with 12 observations on the following variable.
  \describe{
    \item{\code{V1}}{a numeric vector}
  }
}

\source{
  Article: Simultaneous selection for yield and stability. Kang.
}
\references{
  Kang, M. S. 1993. Simultaneous selection for yield and stability:
  Consequences for growers. Agron. J. 85:754-757.
}
\examples{
library(agricolae)
data(precipitation)
str(precipitation)
}
\keyword{datasets}
