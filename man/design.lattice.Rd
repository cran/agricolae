\name{design.lattice}
\alias{design.lattice}
%- design.lattice.
\title{ Lattice designs }
\description{
  SIMPLE and TRIPLE lattice designs. It randomizes treatments in k x k lattice.
}
\usage{
design.lattice(k, type="triple", number = 1, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{k}{ order lattice }
  \item{type}{simple or triple lattice}
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
\references{ FIELD PLOT TECHNIQUE. Erwin L. LeCLERG. 2nd ed., 1962, 
 Burgess Publishing Company, Minnesota }

\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.rcbd}}, \code{\link{design.lsd}},
\code{\link{random.ab}}, \code{\link{fact.nk} } }

\examples{
library(agricolae)
design.lattice(10,type="simple") # simple lattice design, 10x10 (100 varieties)
design.lattice(3,type="triple") # triple lattice design,  3x3 ( 9 varieties)
}
\keyword{ design }% at least one, from doc/KEYWORDS

