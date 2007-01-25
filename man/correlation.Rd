\name{correlation}
\alias{correlation}
%- correlation.
\title{ Correlation analysis. Methods of Pearson, Spearman and Kendall }
\description{
  It obtains the coefficients of correlation and p-value between all the
  variables of a data table. The methods to apply are Pearson, Spearman
  and Kendall. In case of not specifying the method, the Pearson method 
  will be used. The results are similar to SAS.
}
\usage{
correlation(x,y=NULL, method = c("pearson", "kendall", "spearman")
,alternative="two.sided")
}

\arguments{
  \item{x}{ table, matrix or vector }
  \item{y}{ table, matrix or vector }
  \item{method}{ "pearson", "kendall", "spearman" }
  \item{alternative}{ "two.sided", "less", "greater" }
}
\details{
  Parameters equal to function cor()
}
\value{
  \item{table }{Numeric}
}
\author{ Felipe de Mendiburu }
\seealso{\code{\link{correl} } }
\examples{
library(agricolae)
# example 1
data(soil)
analysis<-correlation(soil[,2:8],method="pearson")
analysis

# Example 2: correlation between pH, variable 2 and other elements from soil.
data(soil)
attach(soil)
analysis<-correlation(pH,soil[,3:8],method="pearson",alternative="less")
analysis
detach(soil)

# Example 3: correlation between pH and % clay method kendall.
data(soil)
attach(soil)
correlation(pH,clay,method="kendall", alternative="two.sided")
detach(soil)
}
\keyword{ multivariate }% at least one, from doc/KEYWORDS

