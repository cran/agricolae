\name{cv.model}
\alias{cv.model}
%- cv.model.
\title{ Coefficient of the experiment variation }
\description{
  It obtains the coefficient of variation of the experiment obtained by 
  models lm() or aov()

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

\seealso{\code{\link{LSD.test}}, \code{\link{HSD.test}}, 
\code{\link{waller.test}}}

\examples{
# see examples from LSD , Waller-Duncan or HSD and complete with it:
library(agricolae)
# not run
# cv<-cv.model(model)

}
\keyword{ univar }% at least one, from doc/KEYWORDS

