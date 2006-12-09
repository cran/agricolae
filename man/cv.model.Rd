\name{cv.model}
\alias{cv.model}
%- cv.model.
\title{ Coefficient of variation of the experiment }
\description{
  Obtains the coefficient of variation of the experiment,
  obtained by the models lm() or aov()

}
\usage{
cv.model(x)
}

\arguments{
  \item{x}{ object of model lm() or AOV() }
}
\details{
$\frac{sqrt(MSerror)}{mean(x)}\*100$
}
\value{

  \item{x }{object }

}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{LSD}}, \code{\link{HSD}}, \code{\link{waller}}, 
\code{\link{LSD.group}}, \code{\link{HSD.group}}, \code{\link{waller.group} } }

\examples{
# see examples from LSD , Waller-Duncan or HSD and complete with it:
library(agricolae)
# not run
# cv<-cv.model(model)

}
\keyword{ univar }% at least one, from doc/KEYWORDS

