\name{correl}
\alias{correl}
%- correl.
\title{ Correlation two vectors }
\description{
   Exact correlation for ties or without ties; methods of Kendall,
   Spearman and Pearson.
}
\usage{
correl(x, y, method = "pearson",alternative="two.sided")
}

\arguments{
  \item{x}{ Vector }
  \item{y}{ Vector }
  \item{method}{ "pearson", "kendall", "spearman" }
  \item{alternative}{ "two.sided", "less", "greater" }
}

\value{
  \item{x }{Numeric}
  \item{y }{Numeric}
  ...
}
\references{Numerical Recipes in C. Second Edition. }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{cor.matrix}}, \code{\link{cor.vector}}, \code{\link{cor.mv} } }

\examples{
library(agricolae)
data(soil)
attach(soil)
pH<-soil[,2] ; clay <- soil[,11]
correl(pH,clay,method="kendall")
correl(pH,clay,method="spearman")
correl(pH,clay,method="pearson")

}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

