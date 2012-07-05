\name{random.ab}
\alias{random.ab}
\title{ Randomizing a factorial in a block }
\description{
  It generates a randomization in a block.
}
\usage{
random.ab(p, q)
}

\arguments{
  \item{p}{ level factor 1 }
  \item{q}{ level factor 2 }
}
\value{
  \item{p}{ numeric }
  \item{q}{ numeric }
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.rcbd}}, \code{\link{design.lsd}},
\code{\link{design.ab}}, \code{\link{fact.nk} } }

\examples{
# factorial 2A3B in block
library(agricolae)
block.1 <-random.ab(2,3)
}
\keyword{ design }% at least one, from doc/KEYWORDS

