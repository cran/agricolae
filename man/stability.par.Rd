\name{stability.par}
\alias{stability.par}
%- stability.par.
\title{ Stability analysis. SHUKLA'S STABILITY VARIANCE AND KANG'S }
\description{
  This procedure calculates the variations of stability as well as
  the statistics of selection for the yield and the stability.
  The averages of the genotype through the environment repetitions different
  are required for the calculations. The mean square error must calculate
  of the joint analysis of variancia.
}
\usage{
stability.par(data, rep, MSerror, alpha = 0.1, main = NULL, cova = FALSE, name.cov = NULL, file.cov = 0)
}

\arguments{
  \item{data}{ matrix of averages, by rows the genotypes and columns the environment }
  \item{rep}{ Number of repetitions }
  \item{MSerror}{ Mean Square Error }
  \item{alpha}{ Label significant }
  \item{main}{ Title }
  \item{cova}{ Covariable }
  \item{name.cov}{ Name covariable }
  \item{file.cov}{ Data covariable }
}

\value{
  \item{data}{ Numeric }
  \item{rep}{ Constant numeric }
  \item{MSerror}{ Constant numeric }
  \item{alpha}{ Constant numeric }
  \item{main}{ Text }
  \item{cova}{ FALSE or TRUE }
  \item{name.cov}{ Text }
  \item{file.cov}{ Vector numeric }
}
\references{ Kang, M. S. 1993. Simultaneous selection for yield and
 stability: Consequences for growers. Agron. J. 85:754-757 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{stability.nonpar} } }

\examples{
library(agricolae)
#example 1 without covariable.
data(stable)
stability.par(data=stable, rep=4, MSerror=1.8, alpha=0.1, main="Nativa")

#example 2 covariable. precipitation
data(stable)
data(precipitation)
stability.par(data=stable, rep=4, MSerror=1.8, alpha=0.1, main="Nativa", cova=TRUE, name.cov="Precipitación", file.cov=precipitation)

}
\keyword{ models }% at least one, from doc/KEYWORDS

