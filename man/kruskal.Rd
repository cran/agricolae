\name{kruskal}
\alias{kruskal}
%- kruskal.
\title{ Kruskal Wallis test and multiple comparison of treatments. }
\description{
 It makes the multiple comparison with Kruskal-Wallis. The parameters
 by default are alpha = 0.05.
}
\usage{
kruskal(y, trt, alpha = 0.05, group=TRUE, main = NULL)
}
\arguments{
  \item{y}{ response }
  \item{trt}{ treatment }
  \item{alpha}{ level signification }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
}
\value{
  \item{y}{ vector numeric }
  \item{trt}{ vector alphanumeric }
  \item{alpha}{ level significant }
  \item{group}{ Logic }
  \item{main}{ Title }
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }

\seealso{ \code{\link{friedman}}, \code{\link{durbin.test}} }

\examples{
library(agricolae)
data(corn)
attach(corn)
str(corn)
comparison<-kruskal(observation,method,group=TRUE, main="corn")
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

