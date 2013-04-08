\name{reg.homog}
\alias{reg.homog}
%- reg.homog.
\title{ Homologation of regressions }
\description{
  It makes the regressions homogeneity test for a group of treatments where
  each observation presents a linearly dependent reply from another one. 
  There is a linear function in every treatment.
  The objective is to find out if the linear models of each treatment
  come from the same population.
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
\references{ Book in Spanish: Metodos estadisticos para la investigacion.
             Calzada Benza 1960 }
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
data(frijol)
attach(frijol)
evaluation<-reg.homog(technology,production,index)
detach(frijol)
}
\keyword{ regression }% at least one, from doc/KEYWORDS

