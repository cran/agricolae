\name{order.stat}
\alias{order.stat}
%- order.stat.
\title{ Grouping the treatments averages in a comparison with a minimum value  }
\description{
  When there are treatments and their respective values, these can be 
  compared with a minimal difference of meaning.
}
\usage{
order.stat(treatment, means, minimum,console=TRUE)
}

\arguments{
  \item{treatment}{ treatment }
  \item{means}{ means of treatment }
  \item{minimum}{ minimum value for the comparison }
  \item{console}{logical, print output }
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

