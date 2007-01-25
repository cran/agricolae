\name{design.bib}
\alias{design.bib}
%- design.bib.
\title{ Randomized Balanced Incomplete Block Designs. BIB}
\description{
  It generates Randomized Balanced Incomplete Block Design.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
design.bib(trt, k, number = 1, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{trt}{ Treatments}
  \item{k}{ size block }
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
  \item{k }{number, numeric}
  \item{number }{Numeric}
  \item{seed }{Numeric}
  \item{kinds}{ character }
}
\references{ Design of Experiments. Robert O. Kuehl. 2nd ed., Duxbury, 2000 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.lsd}}, \code{\link{random.ab}}, 
\code{\link{design.rcbd} },\code{\link{fact.nk} } }

\examples{
library(agricolae)
# 4 treatments and k=3 size block
trt<-c("A","B","C","D")
k<-3
bib <-design.bib(trt,k,number=101,seed =41,kinds ="Super-Duper") # seed = 37
plots <-as.numeric(bib[,1])
field <-as.character(bib[,3])
t(matrix(plots,c(3,4)))
t(matrix(field,c(3,4)))
# write in hard disk
# write.table(bib,"bib.txt", row.names=FALSE, sep="\t")
# file.show("bib.txt")

}
\keyword{ design }% at least one, from doc/KEYWORDS

