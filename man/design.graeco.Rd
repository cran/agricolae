\name{design.graeco}
\alias{design.graeco}
%- design.graeco.
\title{ Graeco - latin square design}
\description{
  A graeco - latin square is a KxK pattern that permits the study of k 
  treatments simultaneously with three different blocking variables, each 
  at k levels.
  
  The function is only for squares of the odd numbers and even numbers (4, 8, 10 and 12)
}
\usage{
design.graeco(trt1, trt2, number = 1, seed = 0, kinds = "Super-Duper")
}

\arguments{
  \item{trt1}{ Treatments }
  \item{trt2}{ Treatments }
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
  \item{trt1 }{vector, name of the treatments}
  \item{trt2 }{vector, name of the treatments}  
  \item{number }{Numeric}
  \item{seed }{Numeric}
}
\references{ 
1. Statistics for Experimenters Design, Innovation, and Discovery
Second Edition. George E. P. Box. Wiley-Interscience. 2005.

2. Experimental design. Cochran and Cox. Second edition. 
Wiley Classics Library Edition published 1992.
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{design.crd}}, \code{\link{design.lsd}}, \code{\link{random.ab}}, 
\code{\link{fact.nk} } }

\examples{
library(agricolae)
T1<-c("a","b","c","d")
T2<-c("v","w","x","y")
graeco <-  design.graeco(T1,T2,number=101)
plots <-as.numeric(graeco[,1])
trt <- paste(graeco[,4],graeco[,5])
dim(plots)<-c(4,4)
dim(trt) <-c(4,4)
print(t(plots))
print(t(trt))
# 10 x 10
T1 <- letters[1:10]
T2 <- 1:10
graeco <-  design.graeco(T1,T2)
trt <- paste(graeco[,4],graeco[,5])
dim(trt) <-c(10,10)
print(t(trt))
}
\keyword{ design }

