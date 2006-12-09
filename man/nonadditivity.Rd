\name{nonadditivity}
\alias{nonadditivity}
%- nonadditivity.
\title{ Test nonadditivity model }
\description{
  The resistance for the transformable nonaditividad, due to J. W. Tukey, 
  is based on the detection of a curvilinear relation between y-est(y)
  and est(y). A degree of freedom for the transformable non-additivity.
}
\usage{
nonadditivity(y, factor1, factor2, df, MSerror)
}

\arguments{
  \item{y}{ Answer of the experimental unit  }
  \item{factor1}{ Firts treatment applied to each experimental unit }
  \item{factor2}{ Second treatment applied to each experimental unit }
  \item{df}{ Degrees of freedom of the experimental error }
  \item{MSerror}{ Means square error of the experimental }
}
\details{
Only two factor: Block and treatment or factor 1 and factor 2.
}
\value{
  \item{y }{Numeric}
  \item{factor1 }{alfanumeric}
  \item{factor2 }{alfanumeric}
  \item{df }{Numeric}
  \item{MSerror }{Numeric}

}
\references{ Statistics for experimenters. George E.P. Box, J. Stuart Hunter
 and William G. Hunter. Wile Series in probability and statistics }
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
data(potato )
potato[,1]<-as.factor(potato[,1])
model<-lm(cutting ~ date + variety,potato)
df<-df.residual(model)
MSerror<-deviance(model)/df
attach(potato)
analysis<-nonadditivity(cutting, date, variety, df, MSerror)
detach(potato)
}
\keyword{ models  }% at least one, from doc/KEYWORDS

