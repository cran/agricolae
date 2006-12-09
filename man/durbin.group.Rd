\name{durbin.group}
\alias{durbin.group}
%- durbin.group.
\title{ Durbin test and multiple comparison of treatments }
\description{
  Multiple comparison of the Durbin test for the balanced incomplete 
  blocks for sensorial or categorical evaluation. Forms groups according
  to a the asked for level of significance (alpha), by default 0.05.
}
\usage{
durbin.group(judge, trt, evaluation, k, r, alpha = 0.05, main = NULL)
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

\seealso{\code{\link{durbin.comp}}, \code{\link{kruskal.comp}}, \code{\link{friedman.comp}}, 
\code{\link{kruskal.group}}, \code{\link{friedman.group} } }
\examples{
library(agricolae)
data(sensorial)
attach(sensorial)
comparison<-durbin.group(judge,trt,texture,k=5,r=3,alpha=0.01,main="Clay")
#startgraph
bar.group(comparison,horiz=TRUE,xlim=c(0,20),density=4)
#endgraph
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

