\name{order.stat}
\alias{order.stat}
%- order.stat.
\title{ Orders groups of treatments in a multiple comparison
 for a minimum value of difference }
\description{
  When one has treatments and values that represent it,
  these can be compared with a difference minimal of meaning.
}
\usage{
order.stat(treatment, means, minimum)
}

\arguments{
  \item{treatment}{ treatment }
  \item{means}{ means of treatment }
  \item{minimum}{ minimum value for the comparison }
}

\value{
  \item{trt}{ Factor }
  \item{means}{ Numeric }
  \item{minimum}{ Numeric }
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{order.group} } }

\examples{
library(agricolae)
treatments <- c("A","B","C","D","E","F")
means<-c(2,5,3,7,9,5)
minimum.diff <- 2
groups<-order.stat(treatments,means,minimum.diff)

}
\keyword{ manip }% at least one, from doc/KEYWORDS

