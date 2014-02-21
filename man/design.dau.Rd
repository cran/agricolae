\name{design.dau}
\alias{design.dau}
%- design.dau.
\title{ Augmented block design }
\description{
  In these designs, some treatments (checks) are replicate r times and
  other treatments (new) are replicated once.
}
\usage{
design.dau(trt1, trt2, r, serie = 2, seed = 0, kinds = "Super-Duper", name="trt")
}

\arguments{
  \item{trt1}{ checks }
  \item{trt2}{ new }
  \item{r}{ Replications or blocks }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
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
  \item{parameters }{list}
  \item{book }{dataframe}
}
\references{
1. Augmented (or Hoonuiaku) Design. Federer, W.T. (1956), Hawaii Plr. rec., 55: 191-208.
2. In Augmented Designs. Federer, W.T and Raghavarao, D. (1975). Bometrics, vol. 31, No. 1 (mar.., 1975), pp. 29-35
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.bib}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.split} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
# 4 treatments and 5 blocks
T1<-c("A","B","C","D")
T2<-letters[20:26]
outdesign <-design.dau(T1,T2, r=5,serie=2)
# field book
book<-outdesign$book
by(book,book[2],function(x) paste(x[,1],"-",as.character(x[,3])))
# write in hard disk
# write.table(book,"dau.txt", row.names=FALSE, sep="\t")
# file.show("dau.txt")
# Augmented designs in Completely Randomized Design
trt<-c(T1,T2)
r<-c(4,4,4,4,1,1,1,1,1,1,1)
outdesign <- design.crd(trt,r)
outdesign$book
}
\keyword{ design }
