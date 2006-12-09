\name{cor.matrix}
\alias{cor.matrix}
%- cor.matrix.
\title{ Analysis of correlation. Methods of Pearson, Spearman and Kendall }
\description{
  Obtains the coefficients of correlation and p-value between all the
  variables of a data table. The methods to apply are Pearson, Spearman
  and Kendall. Default is Pearson. Results similar to SAS.

}
\usage{
cor.matrix(table, method = c("pearson", "kendall", "spearman")
,alternative="two.sided")
}

\arguments{
  \item{table}{ Variables from correlation. A data frame. }
  \item{method}{ "pearson", "kendall", "spearman" }
  \item{alternative}{ "two.sided", "less", "greater" }
}
\details{
  Parameters equal to function cor()
}
\value{
  \item{table }{Numeric}
}
\references{ Experimental field }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{cor.vector}}, \code{\link{cor.mv}}, \code{\link{correl} } }

\examples{
library(agricolae)
data(soil)
correlation<-cor.matrix(soil[,-1],method="pearson")
as.dist(correlation$correlation)
as.dist(correlation$pvalue)
}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

