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
design.rcbd(trt, r, serie = 2, seed = 0, kinds = "Super-Duper", first=FALSE)
}

\arguments{
  \item{trt}{ Treatments}
  \item{r}{ Replications or blocks }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
  \item{seed}{ seed }
  \item{kinds}{ method for to randomize }
  \item{first}{ TRUE or FALSE - randomize rep 1}
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
  \item{kinds}{ character }
  \item{first }{Logic}
}
\references{ Introduction to Experimental Statistics. 
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1964 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.split}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
# 5 treatments and 6 blocks
trt<-c("A","B","C","D","E")
rcbd <-design.rcbd(trt,6,serie=2,986,"Wichmann-Hill") # seed = 986
rcbd # field book
# write in hard disk
# write.table(rcbd,"rcbd.txt", row.names=FALSE, sep="\t")
# file.show("rcbd.txt")
# Plots in field model ZIGZAG
plots <-as.numeric(rcbd[,1])
block<- as.numeric(rcbd[,2])
trt <-as.character(rcbd[,3])
nplots<- length(plots)
nblock<- nlevels(rcbd[,2])
ntrt<- nlevels(rcbd[,3])
indice<-1:nplots
for (i in 1:nplots){
if ( 2*floor(block[i]/2) == block[i])indice[i]<-2*block[i]*ntrt-i-ntrt+1
}
plots<-plots[indice]
trt<-trt[indice]
dim(plots)<-c(ntrt,nblock)
dim(trt)<-c(ntrt,nblock)
print(t(plots))
print(t(trt))
}
\keyword{ design }% at least one, from doc/KEYWORDS

