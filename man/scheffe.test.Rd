\name{scheffe.test}
\alias{scheffe.test}
%- scheffe.test.
\title{ Multiple comparisons, scheffe }
\description{
  Scheffe 1959, method is very general in that all possible contrasts 
  can be tested for significance and confidence intervals can be 
  constructed for the corresponding linear. The test is conservative. 
  }
\usage{
scheffe.test(y, trt, DFerror, MSerror, Fc, alpha = 0.05, group=TRUE, main = NULL,
console=FALSE )
}
\arguments{
  \item{y}{ model(aov or lm) or answer of the experimental unit  }
  \item{trt}{ Constant( only y=model) or vector treatment applied to each experimental unit  }
  \item{DFerror}{ Degrees of freedom }
  \item{MSerror}{ Mean Square Error }
  \item{Fc}{ F Value }
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
  \item{Fc }{Numeric}
  \item{alpha }{Numeric}
  \item{group}{ Logic }  
  \item{main }{Text}
}
\references{ 
Robert O. Kuehl. 2nd ed. Design of experiments. Duxbury, copyright 2000.
Steel, R.; Torri,J; Dickey, D.(1997) Principles and Procedures of Statistics
A Biometrical Approach. pp189
}
\author{ Felipe de Mendiburu}

\seealso{\code{\link{HSD.test}}, \code{\link{LSD.test}}, \code{\link{SNK.test} }, 
\code{\link{bar.err}}, \code{\link{bar.group}},\code{\link{duncan.test} } }

\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
comparison <- scheffe.test(model,"virus", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")
# Old version scheffe.test()
attach(sweetpotato)
df<-df.residual(model)
MSerror<-deviance(model)/df
Fc<-anova(model)["virus",4]
comparison <- scheffe.test(yield, virus, df, MSerror, Fc, group=TRUE, 
main="Yield of sweetpotato. Dealt with different virus")
detach(sweetpotato)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

