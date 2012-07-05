\name{design.strip}
\alias{design.strip}
%- design.strip.
\title{ Strip Plot Design }
\description{
  It generates strip plot design.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
design.strip(trt1, trt2,r, number = 1, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{trt1}{ Row treatments}
  \item{trt2}{ column treatments}
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

  \item{trt1 }{vector, name of the row treatments}
  \item{trt2 }{vector, name of the column treatments}  
  \item{r }{ numeric}
  \item{number }{Numeric}
  \item{seed }{Numeric}
}
\references{ Statistical Procedures for Agricultural Research. 
Kwanchai A. Gomez, Arturo A. Gomez. John Wiley & Sons, new York, 1984 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.lsd}}, \code{\link{random.ab}}, 
\code{\link{fact.nk} } }

\examples{
library(agricolae)
# 4 and 3 treatments and 3 blocks in strip-plot
t1<-c("A","B","C","D")
t2<-c(1,2,3)
r<-3
book <-design.strip(t1,t2,r, number=101,seed=45,kinds ="Super-Duper") # seed = 45
book # field book
# write in hard disk
# write.table(book,"book.txt", row.names=FALSE, sep="\t")
# file.show("book.txt")

}
\keyword{ design }% at least one, from doc/KEYWORDS

