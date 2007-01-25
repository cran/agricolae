\name{design.rcbd}
\alias{design.rcbd}
%- design.rcbd.
\title{ Randomized Complete Block Design }
\description{
  It generates Randomized Complete Block Design.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
design.rcbd(trt, r, number = 1, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{trt}{ Treatments}
  \item{r}{ Replications or blocks }
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
  \item{number }{Numeric}
  \item{seed }{Numeric}
}
\references{ Introduction to Experimental Statistics. 
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1964 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.lsd}}, \code{\link{random.ab}}, 
\code{\link{fact.nk} } }

\examples{
library(agricolae)
# 4 treatments and 5 blocks
trt<-c("A","B","C","D")
rcbd <-design.rcbd(trt,5,number=101,45,"Super-Duper") # seed = 45
rcbd # field book
plots <-as.numeric(rcbd[,1])
trt <-as.character(rcbd[,3])
dim(plots)<-c(4,5)
dim(trt) <-c(4,5)
print(t(plots))
print(t(trt))
# write in hard disk
# write.table(rcbd,"rcbd.txt", row.names=FALSE, sep="\t")
# file.show("rcbd.txt")

}
\keyword{ design }% at least one, from doc/KEYWORDS

