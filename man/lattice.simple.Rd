\name{lattice.simple}
\alias{lattice.simple}
%- lattice.simple.
\title{ Lattice design }
\description{
  It randomizes treatments in a simple adjustment of a k x k lattice.
}
\usage{
lattice.simple(k, number = 1, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{k}{ order lattice }
  \item{number}{ number of first plot }
  \item{seed}{ seed }
  \item{kinds}{ method for to randomize }
}
\details{
   kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
  "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",  "Knuth-TAOCP-2002",
  "default" )
}
\value{
  \item{k }{vector, numeric}
  \item{number }{Numeric}
  \item{seed }{Numeric}
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.rcbd}}, \code{\link{design.lsd}},
\code{\link{random.ab}}, \code{\link{fact.nk} } }

\examples{
# latice 10x10 for 100 treatments
library(agricolae)
lattice.simple(10)
}
\keyword{ design }% at least one, from doc/KEYWORDS

