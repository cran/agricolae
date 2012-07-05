\name{design.cyclic}
\alias{design.cyclic}

\title{ Cyclic designs }
\description{
  The cyclic design is a incomplete blocks designs, it is generated from 
  a incomplete block initial of the size k, the plan is generated and 
  randomized. The efficient and robust cyclic designs for 6 to 30 treatments, 
  replications <= 10. 
}
\usage{
design.cyclic(trt, k, r, number = 1, rowcol = FALSE, seed = 0, kinds = "Super-Duper")
}
\arguments{
  \item{trt}{ vector treatments }
  \item{k}{ block size}
  \item{r}{ Replications }
  \item{number}{ number of first plot }
  \item{rowcol}{ TRUE: row-column design }
  \item{seed}{ init seed random }
  \item{kinds}{ random method }
}
\details{
  Number o treatment 6 to 30.
  (r) Replication 2 to 10.
  (k) size of block 2 to 10.
  replication = i*k, "i" is value integer.
}
\value{
  \item{trt }{vector, name of the treatments}
  \item{k }{Constant, numeric}
  \item{number }{Constant, numeric}
  \item{seed }{Constant, numeric}
  \item{kinds}{ character }
}
\references{ Kuehl, Robert(2000), Design of Experiments. 2nd ed., Duxbury. 
John, J.A. (1981) Efficient Cyclic Design. J. R. Statist. Soc. B, 43, No. 1, pp, 76-80.
}
\author{ Felipe de Mendiburu }
\seealso{\code{\link{design.bib}}, \code{\link{design.lattice}}, \code{\link{design.alpha}} }
\examples{
library(agricolae)
trt<-letters[1:8]
# block size = 2, replication = 6
plan1 <- design.cyclic(trt,k=2, r=6)
names(plan1)
# groups 1,2,3
plan1$design[[1]]
plan1$design[[2]]
plan1$design[[3]]
plan1$book
# row-column design
plan2 <- design.cyclic(trt,k=2, r=6, rowcol=TRUE)
plan2$design
}
\keyword{ design  }
