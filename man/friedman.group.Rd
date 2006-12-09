\name{friedman.group}
\alias{friedman.group}
%- friedman.group.
\title{ Friedman Test and multiple comparison of treatments, group }
\description{
  The data consist of b mutually independent k-variate random variables
  called b blocks. The random variable is in block and associated with
  treatment. It makes the multiple comparison of the test of Friedman
  with or without ties. A first result is obtained by friedman.test of R.
}
\usage{
friedman.group(judge, evaluation, alpha = 0.05, main = NULL)
}

\arguments{
  \item{judge}{ Identification of the judge in the evaluation }
  \item{evaluation}{ Variable. The names of the columns are the treatments  }
  \item{alpha}{ Significant test }
  \item{main}{ Title }
}  

\value{

  \item{judge }{Vector}
  \item{evaluation }{Matrix or data.frame }
  \item{alpha}{ Numeric }
  \item{main}{ Text }
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{friedman.comp}}, \code{\link{kruskal.comp}}, \code{\link{kruskal.group}}, 
\code{\link{durbin.comp} }, \code{\link{durbin.group} } }

\examples{
library(agricolae)
data(friedman)
judge<-friedman[,1]
evaluation<-friedman[,-1]
comparison<-friedman.group(judge,evaluation,alpha=0.05,main="Data of the book of Conover")
#startgraph
bar.group(comparison,density=3,border="red",col="blue",ylim=c(0,45))
#endgraph
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

