\name{HSD.test}
\alias{HSD.test}
%- HSD.
\title{ Multiple comparisons: Tukey  }
\description{
  It makes multiple comparisons of treatments by means of Tukey.
  The level by alpha default is 0.05.
}
\usage{
HSD.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL)
}

\arguments{
  \item{y}{ Variable response }
  \item{trt}{ Treatments }
  \item{DFerror}{ Degree free }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Significant level }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
}
\details{
  It is necessary first makes a analysis of variance.
}
\value{

  \item{y }{Numeric}
  \item{trt }{factor}
  \item{DFerror }{Numeric}
  \item{MSerror }{Numeric}
  \item{alpha }{Numeric}
  \item{group}{ Logic}  
  \item{main }{Text}

}
\references{ Principles and procedures of statistics a biometrical approach
             Steel & Torry & Dickey. Third Edition 1997 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{LSD.test}}, \code{\link{waller.test} } }

\examples{
library(agricolae)
data(sweetpotato)
attach(sweetpotato)
model<-aov(yield~virus)
df<-df.residual(model)
MSerror<-deviance(model)/df
comparison <- HSD.test(yield,virus,df,MSerror, group=TRUE,
main="Yield of sweetpotato. Dealt with different virus")
detach(sweetpotato)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

