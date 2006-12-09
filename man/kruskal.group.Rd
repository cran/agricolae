\name{kruskal.group}
\alias{kruskal.group}
%- kruskal.group.
\title{ Kruskal Wallis test and multiple comparison of treatments. groups }
\description{
 It makes the multiple comparison with Kruskal-Wallis. The parameters
  by default are alpha = 0.05 and ties = 0.
}
\usage{
kruskal.group(y, trt, ties = 0, alpha = 0.05, main = NULL)
}
\arguments{
  \item{y}{ response }
  \item{trt}{ treatment }
  \item{ties}{ ties }
  \item{alpha}{ level signification }
  \item{main}{ Title }
}
\value{
  \item{y}{ vector numeric }
  \item{trt}{ vector alphanumeric }
  \item{ties}{ 0 (not ties) or 1 (yes ties) }
  \item{alpha}{ level significant }
  \item{main}{ Title }
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{friedman.comp}}, \code{\link{friedman.group}}, \code{\link{kruskal.comp}},
\code{\link{durbin.comp}}, \code{\link{durbin.group}} }

\examples{
library(SuppDists)
library(agricolae)
data(kruskal)
attach(kruskal)
comparison<-kruskal.group(obs,trt) # whitout ties 
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

