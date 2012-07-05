\name{design.dau}
\alias{design.dau}
%- design.dau.
\title{ Augmented block design }
\description{
  In these designs, some treatments (checks) are replicate r times and
  other treatments (new) are replicated once.
}
\usage{
design.dau(trt1, trt2, r, number = 1, seed = 0, kinds = "Super-Duper", name="trt")
}

\arguments{
  \item{trt1}{ checks }
  \item{trt2}{ new }
  \item{r}{ Replications or blocks }
  \item{number}{ number of first plot }
  \item{seed}{ seed }
  \item{kinds}{ method for to randomize }
  \item{name}{ name of treatments}
}
\details{
  kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
  "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",  "Knuth-TAOCP-2002",
  "default" )
}
\value{
  \item{trt1 }{vector, name of the treatments}
  \item{trt2 }{vector, name of the treatments}
  \item{r }{Numeric}
  \item{number }{Numeric}
  \item{seed }{Numeric}
}
\references{
1. Augmented (or Hoonuiaku) Design. Federer, W.T. (1956), Hawaii Plr. rec., 55: 191-208.
2. In Augmented Designs. Federer, W.T and Raghavarao, D. (1975). Bometrics, vol. 31, No. 1 (mar.., 1975), pp. 29-35
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.rcbd}} }

\examples{
library(agricolae)
# 4 treatments and 5 blocks
T1<-c("A","B","C","D")
T2<-letters[20:26]
dau <-design.dau(T1,T2, r=5)
# field book
dau
by(dau,dau[2],function(x) paste(x[,1],"-",as.character(x[,3])))
# write in hard disk
# write.table(dau,"dau.txt", row.names=FALSE, sep="\t")
# file.show("dau.txt")
# Augmented designs in Completely Randomized Design
trt<-c(T1,T2)
r<-c(4,4,4,4,1,1,1,1,1,1,1)
book <- design.crd(trt,r)
}
\keyword{ design }
