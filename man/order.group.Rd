\name{order.group}
\alias{order.group}
%- order.group.
\title{ Orders groups of treatments in a multiple comparison }
\description{
  When one has treatments and their averages 
  these can be compared with a value that is the minimal significant difference,
  this difference can change from one comparison to another one.
  Function used by HSD, LSD, Kruskal-Wallis, Friedman, Durbin.
}
\usage{
order.group(trt, means, N, MSerror, Tprob, std.err, parameter=1)
}

\arguments{
  \item{trt}{ Treatments }
  \item{means}{ Means of treatment }
  \item{N}{ Replications }
  \item{MSerror}{ Mean square error }
  \item{Tprob}{ minimum value for the comparison }
  \item{std.err}{ standard error }
  \item{parameter}{ Constante 1 (Sd), 0.5 (Sx) }
}
\value{
  \item{trt}{ Factor }
  \item{means}{ Numeric }
  \item{N}{ Numeric }
  \item{MSerror}{ Numeric }
  \item{Tprob}{ value between 0 and 1 }
  \item{std.err}{ Numeric }
  \item{parameter}{ Constant }
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{order.stat} } }
\examples{
library(agricolae)
treatments <- c("A","B","C","D","E","F")
means<-c(20,40,35,72,49,58)
std.err<-c(1.2, 2, 1.5, 2.4, 1, 3.1)
replications <- c(4,4,3,4,3,3)
MSerror <- 55.8
value.t <- 2.1314
groups<-order.group(treatments,means,replications,MSerror,value.t,std.err)
groups
bar.err(groups,ylim=c(0,80))
}
\keyword{ manip }% at least one, from doc/KEYWORDS

