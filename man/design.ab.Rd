\name{design.ab}
\alias{design.ab}
%- design.ab.
\title{ Design in blocks for factorial pxq }
\description{
  Generates design of blocks for combined pxq.
  "Random" uses the methods of generation of numbers of "base" R,
  the seed is by set.seed(seed, kinds).

}
\usage{
design.ab(A, B, r, number = 1, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{A}{ Levels of A }
  \item{B}{ Levels of B }
  \item{r}{ Replications or Blocks }
  \item{number}{ Number of first plot }
  \item{seed}{ Seed }
  \item{kinds}{ Method for to randomize }
}
\details{
  kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
  "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",  "Knuth-TAOCP-2002",
  "default" )
}
\value{
  \item{A }{vector, name of the levels of the first factor}
  \item{B }{vector, name of the levels of the second factor}
  \item{r }{Numeric}
  \item{number }{Numeric}
  \item{seed }{Numeric}
}
\references{ Introduction to Experimental Statistics. 
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1964 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.rcbd}}, \code{\link{design.lsd}},  
\code{\link{random.ab}}, \code{\link{fact.nk}} }

\examples{
# factorial 3 x 3 with 5 replications
# 3 varieties from potato and nitrogen in the fertilization, kg/ha
library(agricolae)
variety <- c("perricholi","canchan","tomasa")
nitrogen <- c(40,80,120) # level of nitrogen
rcbd.ab <-design.ab(variety, nitrogen, 5, number=1001)
print(rcbd.ab) # print of the field book

}
\keyword{ design }% at least one, from doc/KEYWORDS

