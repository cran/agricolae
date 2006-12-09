\name{cor.mv}
\alias{cor.mv}
%- cor.mv.
\title{ Correlation between groups of variables }
\description{
  It obtains the coefficients of correlation between two groups of variables,
  along with p-value, by means of the methods of Pearson, Spearman or Kendall.
  By default it is Pearson. Results similar to the SAS

}
\usage{
cor.mv(aa, bb, method = c("pearson", "kendall", "spearman")
,alternative="two.sided")
}

\arguments{
  \item{aa}{ first group. Vector o data frame }
  \item{bb}{ Second group. Vector or data frame }
  \item{method}{ "pearson", "kendall", "spearman" }
  \item{alternative}{ "two.sided", "less", "greater" }
}
\details{
  Parameters equal to function cor()
}
\value{
  \item{aa }{Numeric}
  \item{bb }{Numeric}
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{cor.vector}}, \code{\link{cor.matrix}}, \code{\link{correl} } }

\examples{
library(agricolae)
data(soil)
# correlation between pH, variable 2 and other elements from soil.
correlation<-cor.mv(soil[,2],soil[,c(-1,-2)],method="pearson")
correlation
}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

