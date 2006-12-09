\name{durbin.comp}
\alias{durbin.comp}
%- durbin.comp.
\title{ Test of Durbin and multiple comparison of treatments }
\description{
  Multiple comparisons using a Durbin test for the balanced incomplete
  blocks for sensorial or categorical evaluation.
}
\usage{
durbin.comp(judge, trt, evaluation, k, r, alpha = 0.05, main = NULL)
}

\arguments{
  \item{judge}{ Identification of the judge in the evaluation }
  \item{trt}{ Treatments }
  \item{evaluation}{ variable }
  \item{k}{ Number of evaluations by judge }
  \item{r}{ Number of replications of the treatment }
  \item{alpha}{ level of significant }
  \item{main}{ Title }
}
\details{
The balanced Incomplete blocks Design.  
}
\value{
  \item{juege }{Vector, numeric}
  \item{trt }{Vector, numeric}
  \item{evaluation }{Vector, numeric}
  \item{k }{Vector, numeric}
  \item{r }{Vector, numeric}
  \item{alpha }{Vector, numeric, default is 0.05}
  \item{main}{text }
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{kruskal.comp}}, \code{\link{friedman.comp}}, \code{\link{kruskal.group}}, 
\code{\link{friedman.group}}, \code{\link{durbin.group} } }

\examples{
library(agricolae)
data(sensorial)
attach(sensorial)
comparison<-durbin.comp(judge,trt,texture,k=5,r=3,alpha=0.01,main="Clay")
comparison

}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

