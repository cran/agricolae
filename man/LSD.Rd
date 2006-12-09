\name{LSD}
\alias{LSD}
%- LSD.
\title{ Multiple comparisons "Least significant difference" LSD }
\description{
  Multiple comparisons of treatments by means of LSD.
  The level of meaning for the comparison by default is 0.05
}
\usage{
LSD(y, trt, df, MSerror, alpha = 0.05, main = NULL)
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

\seealso{\code{\link{LSD.group}}, \code{\link{HSD}}, \code{\link{waller} } }

\examples{
library(agricolae)
data(sweetpotato)
attach(sweetpotato)
model<-aov(yield~virus)
df<-df.residual(model)
MSerror<-deviance(model)/df
comparison<- LSD(yield,virus,df,MSerror,alpha=0.05,main="Yield of sweetpotato")
}
\keyword{ htest }% at least one, from doc/KEYWORDS

