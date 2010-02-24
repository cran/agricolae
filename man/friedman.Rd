\name{friedman}
\alias{friedman}
%- friedman.
\title{ Friedman test and multiple comparison of treatments }
\description{
  The data consist of b mutually independent k-variate random variables
  called b blocks. The random variable is in a block and is associated with
  treatment. It makes the multiple comparison of the Friedman test with or 
  without ties. A first result is obtained by friedman.test of R.
}
\usage{
friedman(judge,trt,evaluation,alpha=0.05,group=TRUE,main=NULL)
}

\arguments{
  \item{judge}{ Identification of the judge in the evaluation }
  \item{trt}{ Treatment }
  \item{evaluation}{ Variable }
  \item{alpha}{ Significant test }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
}  

\value{

  \item{judge }{Vector}
  \item{trt }{Vector }
  \item{evaluation }{Vector }
  \item{alpha}{ Numeric }
  \item{group}{ Logic }
  \item{main}{ Text }
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }

\seealso{ \code{\link{kruskal}}, \code{\link{durbin.test}} }

\examples{
library(agricolae)
data(grass)
attach(grass)
comparison<-friedman(judge,trt, evaluation,alpha=0.05, group=TRUE,
main="Data of the book of Conover")
#startgraph
bar.group(comparison,density=3,border="red",col="blue",ylim=c(0,45))
#endgraph
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

