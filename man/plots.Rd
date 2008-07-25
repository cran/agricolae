\name{plots}
\alias{plots}
\docType{data}
\title{ Data for an analysis in split-plot}
\description{
  Experimental data in blocks, factor A in plots and factor B in sub-plots.
}
\usage{data(plots)}
\format{
  A data frame with 18 observations on the following 5 variables.
  \describe{
    \item{\code{block}}{a numeric vector}
    \item{\code{plot}}{a factor with levels \code{p1} \code{p2} \code{p3} \code{p4} \code{p5} \code{p6}}
    \item{\code{A}}{a factor with levels \code{a1} \code{a2}}
    \item{\code{B}}{a factor with levels \code{b1} \code{b2} \code{b3}}
    \item{\code{yield}}{a numeric vector}
  }
}

\source{
  International Potato Center. CIP
}

\examples{
library(agricolae)
data(plots)
str(plots)
plots[,1] <-as.factor(plots[,1])
# split-plot analysis
model <- aov(yield ~ block + A + Error(plot)+ B + A:B, data=plots)
summary(model)
}
\keyword{datasets}
