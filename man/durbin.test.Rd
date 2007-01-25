\name{durbin.test}
\alias{durbin.test}
%- durbin.test.
\title{ Durbin test and multiple comparison of treatments }
\description{
  A multiple comparison of the Durbin test for the balanced incomplete 
  blocks for sensorial or categorical evaluation. It forms groups according
  to the demanded ones for level of significance (alpha); by default, 0.05.
}
\usage{
durbin.test(judge, trt, evaluation, alpha = 0.05, group =TRUE, main = NULL)
}
\arguments{
  \item{judge}{ Identification of the judge in the evaluation }
  \item{trt}{ Treatments }
  \item{evaluation}{ variable }
  \item{alpha}{ level of significant }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
}
\value{
  \item{juege }{Vector, numeric}
  \item{trt }{Vector, numeric}
  \item{evaluation }{Vector, numeric}
  \item{alpha }{Vector, numeric, default is 0.05}
  \item{group}{ Logic }
  \item{main}{text }
}
\references{ Practical Nonparametrics Statistics. W.J. Conover, 1999
Nonparametric Statistical Methods. Myles Hollander and Douglas A. Wofe, 1999}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{kruskal}}, \code{\link{friedman}},\code{\link{BIB.test}} }
\examples{
library(agricolae)
# Example 1. Conover, pag 391
person<-gl(7,3)
variety<-c(1,2,4,2,3,5,3,4,6,4,5,7,1,5,6,2,6,7,1,3,7)
preference<-c(2,3,1,3,1,2,2,1,3,1,2,3,3,1,2,3,1,2,3,1,2)
comparison<-durbin.test(person,variety,preference,group=TRUE,
main="Seven varieties of ice cream manufacturer")
#startgraph
bar.group(comparison,horiz=TRUE,xlim=c(0,20),density=4)
#endgraph
# Example 2. Myles Hollander, pag 311
# Source: W. Moore and C.I. Bliss. 1942
day<-gl(7,3)
chemical<-c("A","B","D","A","C","E","C","D","G","A","F","G","B","C","F",
 "B","E","G","D","E","F")
toxic<-c(0.465,0.343,0.396,0.602,0.873,0.634,0.875,0.325,0.330,0.423,0.987,
0.426,0.652,1.142,0.989,0.536,0.409,0.309,0.609,0.417,0.931)
comparison<-durbin.test(day,chemical,toxic,group=TRUE,
main="Logarithm of Toxic Dosages")
}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

