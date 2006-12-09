\name{reg.homog}
\alias{reg.homog}
%- reg.homog.
\title{ Homologation of regressions }
\description{
  It makes the test of homogenidad of regressions for a group of treatments
  in where each observation linearly dependent of another one.
  The objective is to know if the linear models of each treatment come from
  a same population.
}
\usage{
reg.homog(trt, y, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trt}{ treatment }
  \item{y}{ dependent variable }
  \item{x}{ independent variable }
}
\value{
  \item{trt}{ factor }
  \item{y}{ numeric }
  \item{x}{ numeric }
}
\references{ Book in Spanish: Métodos estadísticos para la investigación.
             Calzada Benza 1960 }
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
data(homog1)
attach(homog1)
evaluation<-reg.homog(technology,production,index)
}
\keyword{ regression }% at least one, from doc/KEYWORDS

