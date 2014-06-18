\name{Median.test}
\alias{Median.test}
%- median.test.
\title{ Median test. Multiple comparisons }
\description{
  A nonparametric test for several independent samples. The median test is designed
  to examine whether several samples came from populations having the sam median.
  }
\usage{
Median.test(y,trt,correct=TRUE,simulate.p.value=FALSE,console=TRUE)
}
\arguments{
  \item{y}{ Variable response }
  \item{trt}{ Treatments }
  \item{correct}{ a logical indicating whether to apply continuity correction
  when computing the test statistic for 2 groups. The correction will not be bigger
  than the differences themselves. No correction is done if simulate.p.value = TRUE.}
  \item{simulate.p.value}{a logical indicating whether to compute p-values by
  Monte Carlo simulation}
  \item{console}{logical, print output }
}
\details{
The data consist of k samples of posibly unequal sample size.
}
\value{
  \item{statistics }{Numeric}
  \item{parameters }{Numeric}
  \item{Medians }{data.frame}
  \item{comparison}{ data.frame }
  \item{data}{ data.frame }
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{kruskal}},\code{\link{chisq.test}} }

\examples{
library(agricolae)
# example 1
data(corn)
attach(corn)
comparison<-Median.test(observation,method)
comparison
detach(corn)
# example 2
trt<-c(rep(1,9),rep(2,10),rep(3,7),rep(4,8))
y<-c(83,91,94,89,89,96,91,92,90,91,90,81,83,84,83,88,91,89,84,101,100,91,93,
96,95,94,78,82,81,77,79,81,80,81)
comparison<-Median.test(y,trt)
}

\keyword{ nonparametric }% at least one, from doc/KEYWORDS

