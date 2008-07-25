\name{stability.nonpar}
\alias{stability.nonpar}
%- stability.nonpar.
\title{ Nonparametric stability analysis }
\description{
  A method based on the statistical ranges of the study variable per 
  environment for the stability analysis.
}
\usage{
stability.nonpar(data, variable = NULL, ranking = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ First column the genotypes following environment  }
  \item{variable}{ Name of variable }
  \item{ranking}{ print ranking }
}
\value{

  \item{data }{Numeric}
  \item{variable }{Character}
  \item{ranking }{Logical}
}
\references{
Haynes K G, Lambert D H, Christ B J, Weingartner D P, Douches D S,
Backlund J E, Fry W and Stevenson W. 1998. Phenotypic stability of resistance
to late blight in potato clones evaluated at eight sites in the United States
American Journal Potato Research 75, pag 211-217.
 }
\author{ Felipe de Mendiburu}

\seealso{\code{\link{stability.par} } }

\examples{
library(agricolae)
data(haynes)
stability.nonpar(haynes,"YIELD",ranking=TRUE)
# Example 2
data(CIC)
data1<-CIC$comas[,c(1,6,7,17,18)]
data2<-CIC$oxapampa[,c(1,6,7,19,20)]
cic <- rbind(data1,data2)
attach(cic)
means <- by(cic[,5], cic[,c(2,1)], function(x) mean(x,na.rm=TRUE))
means <-as.data.frame(means[,])
cic.mean<-data.frame(genotype=row.names(means),means)
cic.mean<-delete.na(cic.mean,"greater")
stability.nonpar(cic.mean)

}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS

