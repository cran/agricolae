\name{SNK.test}
\alias{SNK.test}
%- SNK.
\title{ Student-Newman-Keuls (SNK) }
\description{
  SNK is derived from Tukey, but it is less conservative (finds more differences).
  Tukey controls the error for all comparisons, where SNK only controls for 
  comparisons under consideration. The level by alpha default is 0.05.
}
\usage{
SNK.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
}

\arguments{
  \item{y}{ model(aov or lm) or answer of the experimental unit  }
  \item{trt}{ Constant( only y=model) or vector treatment applied to each experimental unit  }
  \item{DFerror}{ Degree free }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Significant level }
  \item{group}{ TRUE or FALSE }
  \item{main}{ Title }
  \item{console}{logical, print output }
}
\details{
  It is necessary first makes a analysis of variance.
}
\value{

  \item{y }{class (aov or lm) or vector numeric}
  \item{trt }{constant (only y=model) or vector alfanumeric}
  \item{DFerror }{Numeric}
  \item{MSerror }{Numeric}
  \item{alpha }{Numeric}
  \item{group}{ Logic}  
  \item{main }{Text}

}
\references{ Principles and procedures of statistics a biometrical approach
             Steel & Torry & Dickey. Third Edition 1997 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{LSD.test}}, \code{\link{waller.test} }, \code{\link{HSD.test} },
\code{\link{duncan.test} } }

\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus,data=sweetpotato)
comparison <- SNK.test(model,"virus", 
main="Yield of sweetpotato. Dealt with different virus")
SNK.test(model,"virus", group=FALSE)
# version old SNK.test()
attach(sweetpotato)
df<-df.residual(model)
MSerror<-deviance(model)/df
comparison <- SNK.test(yield,virus,df,MSerror, group=TRUE)
print(comparison$groups)
detach(sweetpotato)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

