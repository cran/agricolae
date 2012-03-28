\name{design.lsd}
\alias{design.lsd}
%- design.lsd.
\title{ Latin Square Design }
\description{
  It generates Latin Square Design.
  "Random" uses the methods of number generation in R.
  The seed is by set.seed(seed, kinds).
}
\usage{
design.lsd(trt, number = 1, seed = 0, kinds = "Super-Duper",first=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trt}{ Treatments }
  \item{number}{ number of first plot }
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
  \item{trt }{Vector names of treatments}
  \item{number }{Numeric}
  \item{seed }{Numeric}
  \item{kinds}{ character }
  \item{first }{Logic}
}
\references{ Introduction to Experimental Statistics. 
Ching Chun Li. McGraw-Hill Book Company, INC, New. York, 1969 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.rcbd}}, \code{\link{random.ab}}, 
\code{\link{fact.nk} } }

\examples{
library(agricolae)
varieties<-c("perricholi","yungay","maria bonita","tomasa")
lsd <-design.lsd(varieties,number=1001,seed=23)
lsd # print field book.
plots <-as.numeric(lsd[,1])
trt <-as.character(lsd[,4])
dim(plots)<-c(4,4)
dim(trt) <-c(4,4)
print(t(plots))
print(t(trt))
# Write on hard disk.
# write.table(lsd,"lsd.txt", row.names=FALSE, sep="\t")
# file.show("lsd.txt")
}
\keyword{ design }% at least one, from doc/KEYWORDS

