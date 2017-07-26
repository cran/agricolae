\name{waerden.test}
\alias{waerden.test}
%- waerden.test.
\title{ Multiple comparisons. The van der Waerden (Normal Scores) }
\description{
  A nonparametric test for several independent samples.
}
\usage{
waerden.test(y, trt, alpha=0.05, group=TRUE, main=NULL,console=FALSE)
}
\arguments{
  \item{y}{ Variable response }
  \item{trt}{ Treatments }
  \item{alpha}{ Significant level }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
The data consist of k samples of posibly unequal sample size.\cr
The post hoc test is using the criterium Fisher's least \cr
significant difference (LSD).
}
\value{

  \item{y }{Numeric}
  \item{trt }{factor}
  \item{alpha }{Numeric}
  \item{group}{ Logic }
  \item{main }{text}
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{kruskal}} }

\examples{
library(agricolae)
# example 1
data(corn)
comparison<-with(corn,waerden.test(observation,method,group=TRUE))
comparison<-with(corn,waerden.test(observation,method,group=FALSE))
# example 2
data(sweetpotato)
comparison<-with(sweetpotato,waerden.test(yield,virus,alpha=0.01,group=TRUE))
}

\keyword{ nonparametric }% at least one, from doc/KEYWORDS

