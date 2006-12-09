\name{design.crd}
\alias{design.crd}
%- design.crd.
\title{ Completely Randomized Design }
\description{
  Generates Completely Randomized Design with equal or different repetition.
  "Random" uses the methods of generation of numbers of "base" of R,
  the seed by set.seed(seed, kinds).

}
\usage{
design.crd(trt, r, number = 1, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{trt}{ Treatments}
  \item{r}{ Replications }
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

  \item{trt }{vector, name of the treatments}
  \item{r }{vector, numeric}
  \item{r }{Numeric}
  \item{number }{Numeric}
  \item{seed }{Numeric}
}
\references{ Introduction to Experimental Statistics. 
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1964 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.rcbd}}, \code{\link{design.lsd}}, \code{\link{design.ab}}, 
\code{\link{fact.nk} } }

\examples{
library(agricolae)
cipnumber <-c("CIP-101","CIP-201","CIP-301","CIP-401","CIP-501")
rep <-c(4,3,5,4,3)
# seed = 12543
crd1 <-design.crd(cipnumber,rep,number=101,12543,"Mersenne-Twister")
# no seed
crd2 <-design.crd(cipnumber,rep,number=101)
# write to hard disk
# write.table(crd1,"crd.txt", row.names=FALSE, sep="\t")
# file.show("crd.txt")

}
\keyword{ design }% at least one, from doc/KEYWORDS

