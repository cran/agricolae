\name{LSD.group}
\alias{LSD.group}
%- LSD.group.
\title{ Multiple comparisons, group: "Least significant difference"}
\description{
  Multiple comparisons of treatments by means of LSD
  and grouping of treatments. The level by default alpha is 0.05.
}
\usage{
LSD.group(y, trt, df, MSerror, alpha = 0.05, main = NULL)
}

\arguments{
  \item{y}{ Answer of the experimental unit  }
  \item{trt}{ Treatment applied to each experimental unit  }
  \item{df}{ Degrees of freedom of the experimental error }
  \item{MSerror}{ Means square error of the experimental  }
  \item{alpha}{ Level of risk for the test  }
  \item{main}{ title of the study  }
}
\details{
  For equal or different repetition.
}
\value{
  The meaning is given by * for the level alpha.

  \item{y }{Numeric}
  \item{trt }{alfanumeric}
  \item{df }{Numeric}
  \item{MSerror }{Numeric}
  \item{alpha }{Numeric}
  \item{main }{Numeric}
}
\references{
     Steel, R.; Torri,J; Dickey, D.(1997)
     Principles and Procedures of Statistics
     A Biometrical Approach. pp178.
     }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{LSD}}, \code{\link{HSD}}, \code{\link{waller}}, \code{\link{HSD.group}},
\code{\link{waller.group}}, \code{\link{bar.err}}, \code{\link{bar.group}} }

\examples{
library(agricolae)
data(sweetpotato)
attach(sweetpotato)
model<-aov(yield~virus)
df<-df.residual(model)
MSerror<-deviance(model)/df
comparison <- LSD.group(yield,virus,df,MSerror,main="Yield of sweetpotato\ndealt with different virus")
#stargraph
bar.err(comparison,std=TRUE,ylim=c(0,45),density=4,border="blue")
#endgraph

}
\keyword{ htest }% at least one, from doc/KEYWORDS

