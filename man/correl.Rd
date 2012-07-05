\name{correl}
\alias{correl}
%- correl.
\title{ Correlation Coefficient }
\description{
   An exact correlation for ties or without ties.
   Methods of Kendall, Spearman and Pearson.
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

\seealso{\code{\link{correlation}} }

\examples{
library(agricolae)
data(soil)
attach(soil)
correl(pH,clay,method="kendall")
correl(pH,clay,method="spearman")
correl(pH,clay,method="pearson")

}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

