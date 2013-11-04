\name{kruskal}
\alias{kruskal}
%- kruskal.
\title{ Kruskal Wallis test and multiple comparison of treatments. }
\description{
 It makes the multiple comparison with Kruskal-Wallis. The parameters
 by default are alpha = 0.05.
}
\usage{
kruskal(y, trt, alpha = 0.05, p.adj=c("none","holm", 
"hochberg", "bonferroni", "BH", "BY", "fdr"), group=TRUE, main = NULL,console=FALSE)
}
\arguments{
  \item{y}{ response }
  \item{trt}{ treatment }
  \item{alpha}{ level signification }
  \item{p.adj}{ Method for adjusting p values (see p.adjust)  }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
  For equal or different repetition.
  p.adj = "holm", "hochberg", "bonferroni", "BH", "BY", "fdr".
  see p.adjust()
  p-adj ="none" is t-student.
  p-adj ="hommel" is not applied in this test.
}
\value{
  \item{y}{ vector numeric }
  \item{trt}{ vector alphanumeric }
  \item{alpha}{ level significant }
  \item{p.adj}{ text, see p.adjust  } 
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
comparison<-kruskal(observation,method,p.adj="bon",group=FALSE, main="corn")
detach(corn)
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

