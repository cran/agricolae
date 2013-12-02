\name{design.ab}
\alias{design.ab}
%- design.ab.
\title{ Design of experiments for a factorial }
\description{
  It generates a design of blocks, randomize and latin square for combined n.
  factors uses the methods of number generation in R.The seed is by 
  set.seed(seed, kinds).

}
\usage{
design.ab(trt, r, serie = 2, design=c("rcbd","crd","lsd"),
seed = 0, kinds = "Super-Duper",first=FALSE)
}

\arguments{
  \item{trt}{ n levels factors }
  \item{r}{ Replications or Blocks }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
  \item{design}{ type }
  \item{seed}{ Seed }
  \item{kinds}{ Method for to randomize }
  \item{first}{ TRUE or FALSE - randomize rep 1}
}
\details{
  kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
  "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",  "Knuth-TAOCP-2002",
  "default" )
}
\value{
  \item{trt }{vector, n levels to factors}
  \item{r }{Numeric}
  \item{serie}{Numeric}
  \item{seed }{Numeric}
  \item{design }{character}
  \item{kinds}{character}
  \item{first}{character}
}
\references{ Introduction to Experimental Statistics. 
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1964 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.split}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
# factorial 3 x 2 with 3 blocks
library(agricolae)
trt<-c(3,2) # factorial 3x2
rcbd <-design.ab(trt, r=3, serie=2)
head(rcbd,10) # print of the field book
# factorial 2 x 2 x 2 with 5 replications in completely randomized design.
trt<-c(2,2,2)
crd<-design.ab(trt, r=5, serie=2,design="crd")
print(crd)
# factorial 3 x 3 in latin square design.
trt <-c(3,3)
lsd<-design.ab(trt, serie=2, design="lsd")
print(lsd)
}
\keyword{ design }% at least one, from doc/KEYWORDS

