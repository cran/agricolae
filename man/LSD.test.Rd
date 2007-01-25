\name{LSD.test}
\alias{LSD.test}
%- LSD.group.
\title{ Multiple comparisons, "Least significant difference"}
\description{
  Multiple comparisons of treatments by means of LSD and a grouping of treatments. 
  The level by alpha default is 0.05.
}
\usage{
LSD.test(y, trt, DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL)
}

\arguments{
  \item{y}{ Answer of the experimental unit  }
  \item{trt}{ Treatment applied to each experimental unit  }
  \item{DFerror}{ Degrees of freedom of the experimental error }
  \item{MSerror}{ Means square error of the experimental  }
  \item{alpha}{ Level of risk for the test  }
  \item{group}{ TRUE or FALSE }
  \item{main}{ title of the study  }
}
\details{
  For equal or different repetition.
}
\value{
  The meaning is given by * for the level alpha.

  \item{y }{Numeric}
  \item{trt }{alfanumeric}
  \item{DFerror }{Numeric}
  \item{MSerror }{Numeric}
  \item{alpha }{Numeric}
  \item{group}{ Logic }
  \item{main }{Numeric}
}
\references{
     Steel, R.; Torri,J; Dickey, D.(1997)
     Principles and Procedures of Statistics
     A Biometrical Approach. pp178.
     }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{HSD.test}}, \code{\link{waller.test}}, \code{\link{bar.err}}, 
\code{\link{bar.group}} }

\examples{
library(agricolae)
data(sweetpotato)
attach(sweetpotato)
model<-aov(yield~virus)
df<-df.residual(model)
MSerror<-deviance(model)/df
comparison <- LSD.test(yield,virus,df,MSerror, group=FALSE,
main="Yield of sweetpotato\ndealt with different virus")
#stargraph
bar.err(comparison,std=TRUE,ylim=c(0,45),density=4,border="blue")
#endgraph

}
\keyword{ htest }% at least one, from doc/KEYWORDS

