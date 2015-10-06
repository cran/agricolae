\name{REGW.test}
\alias{REGW.test}
%- REGW.
\title{ Ryan, Einot and Gabriel and Welsch multiple range test }
\description{
  Multiple range tests for all pairwise comparisons, to obtain a confident inequalities multiple range tests.
}   
\usage{
REGW.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
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
\references{ Multiple comparisons theory and methods. Departament of statistics 
 the Ohio State University. USA, 1996. Jason C. Hsu. Chapman Hall/CRC }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{LSD.test}}, \code{\link{waller.test} }, \code{\link{duncan.test}},
 \code{\link{HSD.test} }, \code{\link{SNK.test} } }

\examples{
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus,data=sweetpotato)
comparison <- REGW.test(model,"virus", 
main="Yield of sweetpotato. Dealt with different virus")
print(comparison)
REGW.test(model,"virus",alpha=0.05,console=TRUE,group=FALSE)
}
\keyword{ htest }% at least one, from doc/KEYWORDS

