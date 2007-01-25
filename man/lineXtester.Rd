\name{lineXtester}
\alias{lineXtester}
%- lineXtester.
\title{ Line x Tester Analysis }
\description{
 It makes the Line x Tester Genetic Analysis. It also estimates the general and 
 specific combinatory ability effects and the line and tester genetic contribution.
 }

\usage{
lineXtester(replications, lines, testers, y)
}
\details{
output:
  Standard Errors for combining ability effects.
  Componentes geneticos.
  Variancias.
  Contribucion proporcional.
  ANOVA with parents and crosses.
  ANOVA for line X tester analysis.
  ANOVA for line X tester analysis including parents.
  GCA Effects.
  Lines Effects.
  Testers Effects.
  Standard Errors for Combining Ability Effects.
  Genetic Components.
  Proportional contribution of lines, testers and their interactions.
  to total variance.

}
\arguments{
  \item{replications}{ Replications}
  \item{lines}{ Lines }
  \item{testers}{ Testers }
  \item{y}{ Variable, response }
}

\value{
  \item{replications}{ vector, numeric}
  \item{lines}{ vector, numeric }
  \item{testers}{ vector, numeric }
  \item{y}{ vector, numeric }
}
\references{ Biometrical Methods in Quantitative Genetic Analysis, Singh, Chaudhary. 1979 }
\author{ Felipe de Mendiburu }

\seealso{\code{\link{AMMI} } }
\examples{
# see structure line by testers
library(agricolae)
data(LxT)
str(LxT)
attach(LxT)
analisis<-lineXtester(replication, line, tester, yield)

}
\keyword{ models }% at least one, from doc/KEYWORDS

