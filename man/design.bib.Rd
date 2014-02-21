\name{design.bib}
\alias{design.bib}
%- design.bib.
\title{ Randomized Balanced Incomplete Block Designs. BIB}
\description{
  Creates Randomized Balanced Incomplete Block Design.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
design.bib(trt, k, serie = 2, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{trt}{ Treatments}
  \item{k}{ size block }
  \item{serie}{ number plot, 1: 11,12; 2: 101,102; 3: 1001,1002 }
  \item{seed}{ seed }
  \item{kinds}{ method for to randomize }
}
\details{
  kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
  "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",  "Knuth-TAOCP-2002",
  "default" )
}
\value{
  \item{parameters }{list}
  \item{statistics }{dataframe}
  \item{book }{dataframe}
}
\references{ Design of Experiments. Robert O. Kuehl. 2nd ed., Duxbury, 2000 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.ab}}, \code{\link{design.alpha}},\code{\link{design.split}}, 
\code{\link{design.crd} }, \code{\link{design.cyclic} }, \code{\link{design.dau} },
\code{\link{design.graeco}}, \code{\link{design.lattice}}, \code{\link{design.lsd}},
\code{\link{design.rcbd}}, \code{\link{design.strip}} }

\examples{
library(agricolae)
# 4 treatments and k=3 size block
trt<-c("A","B","C","D")
k<-3
outdesign<-design.bib(trt,k,serie=2,seed =41,kinds ="Super-Duper") # seed = 41
print(outdesign$parameters)
book<-outdesign$book
plots <-as.numeric(book[,1])
field <-as.character(book[,3])
t(matrix(plots,c(3,4)))
t(matrix(field,c(3,4)))
# write in hard disk
# write.csv(book,"book.csv", row.names=FALSE)
# file.show("book.csv")

}
\keyword{ design }% at least one, from doc/KEYWORDS

