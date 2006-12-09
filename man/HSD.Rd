\name{HSD}
\alias{HSD}
%- HSD.
\title{ Multiple comparisons: Tukey  }
\description{
  It makes multiple comparisons of treatments by means of Tukey.
  The level by default alpha is 0.05.
}
\usage{
HSD(y, trt, df, MSerror, alpha = 0.05, main = NULL)
}

\arguments{
  \item{y}{ Variable response }
  \item{trt}{ Treatments }
  \item{df}{ Degree free }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Significant level }
  \item{main}{ Title }
}
\details{
  It is necessary first makes a analysis of variance.
}
\value{

  \item{y }{Numeric}
  \item{trt }{factor}
  \item{df }{Numeric}
  \item{MSerror }{Numeric}
  \item{alpha }{Numeric}
  \item{main }{text}

}
\references{ Principles and procedures of statistics a biometrical approach
             Steel & Torry & Dickey. Third Edition 1997 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{HSD.group}}, \code{\link{LSD}}, \code{\link{waller} } }

\examples{
library(agricolae)
data(sweetpotato)
attach(sweetpotato)
model<-aov(yield~virus)
df<-df.residual(model)
MSerror<-deviance(model)/df
comparison <- HSD(yield,virus,df,MSerror, main="Yield of sweetpotato. Dealt with different virus")
}
\keyword{ htest }% at least one, from doc/KEYWORDS

