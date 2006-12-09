\name{waerden.group}
\alias{waerden.group}
%- waerden.group.
\title{ Multiple comparisons, group: The van der Waerden (Normal Scores) }
\description{
  Test nonparametrics for Several Independent Samples.
}
\usage{
waerden.group(y, trt,alpha=0.05,main=NULL)
}
\arguments{
  \item{y}{ Variable response }
  \item{trt}{ Treatments }
  \item{alpha}{ Significant level }
  \item{main}{ Title }
}
\details{

The data consist of k samples of posibly unequal sample size.

}
\value{

  \item{y }{Numeric}
  \item{trt }{factor}
  \item{alpha }{Numeric}
  \item{main }{text}
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{kruskal.comp}}, \code{\link{kruskal.group}} }

\examples{
library(agricolae)
library(agricolae)
data(kruskal)
attach(kruskal)
comparison<-waerden.group(obs,trt)
}

\keyword{ htest }% at least one, from doc/KEYWORDS

