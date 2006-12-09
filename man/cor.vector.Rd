\name{cor.vector}
\alias{cor.vector}
%- cor.vector.
\title{ Correlation between two variables }
\description{
  Obtains the correlation coefficient and tests of hypothesis
  r = 0, by means of the methods of Pearson, Spearman or Kendall.
  By default is Pearson and alternative two sided. Results similar to SAS.
}
\usage{
cor.vector(x, y, method = c("pearson", "kendall", "spearman")
,alternative="two.sided")
}

\arguments{
  \item{x}{ Vector 1 }
  \item{y}{ Vector 2 }
  \item{method}{ "pearson", "kendall", "spearman" }
  \item{alternative}{ "two.sided", "less", "greater" }
}
\details{
  Parameters equal to function cor()
}
\value{
  \item{x }{Numeric}
  \item{y }{Numeric}
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{cor.matrix}}, \code{\link{cor.mv}}, \code{\link{correl} } }

\examples{
library(agricolae)
data(soil)
# correlation between pH and % clay method kendall.
attach(soil)
pH<-soil[,2] ; clay <- soil[11]
cor.vector(pH,clay,method="kendall", alternative="two.sided")
}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

