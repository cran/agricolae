\name{PBIB.test}
\alias{PBIB.test}

\title{ Analysis of the Partially Balanced Incomplete Block Design  }
\description{
  Analysis of variance PBIB and comparison mean adjusted.
  Applied to resoluble designs: Lattices and alpha design.
}
\usage{
PBIB.test(block,trt,replication,y,k, method="lsd", alpha=0.05)
}

\arguments{
  \item{block}{ blocks }
  \item{trt}{ Treatment }
  \item{replication}{ Replication }  
  \item{y}{ Response  }
  \item{k}{ size block  }  
  \item{method}{ Comparison treatments }
  \item{alpha}{ Significant test }
}
\details{
Method of comparison treatment. 
lsd: least significant difference.
tukey: Honestly significant differente.
}
\value{
  \item{block}{ Vector, consecutive numbers by replication }
  \item{trt}{ Vector }
  \item{replication}{ Vector }  
  \item{y}{ numeric vector}
  \item{k}{ numeric constant }    
  \item{method}{ Character }
  \item{alpha}{ Numeric }
}
\references{
1. Iterative Analysis of Generalizad Lattice Designs.
 E.R. Williams (1977) Austral J. Statistics 19(1) 39-42. 
 
2. Experimental design. Cochran and Cox. Second edition. 
Wiley Classics Library Edition published 1992
} 
\author{ F. de Mendiburu }
\seealso{\code{\link{BIB.test}}, \code{\link{design.alpha}} }
\examples{
library(agricolae)
library(corpcor)
# alpha design 
trt<-1:30
ntr<-length(trt)
r<-2
k<-3
s<-10
obs<-ntr*r
b <- s*r
book<-design.alpha(trt,k,r,seed=5)
book$book[,3]<- gl(20,3)
# dataset
y<-c(5,2,7,6,4,9,7,6,7,9,6,2,1,1,3,2,4,6,7,9,8,7,6,4,3,2,2,1,1,2,
     1,1,2,4,5,6,7,8,6,5,4,3,1,1,2,5,4,2,7,6,6,5,6,4,5,7,6,5,5,4)
dbook<-data.frame(book$book,yield=y)
rm(y,trt)
# analysis
attach(dbook)
model <- PBIB.test(block, trt, replication, yield, k=3)
detach(dbook)
# model$comparison
# model$means

}
\keyword{ models }

