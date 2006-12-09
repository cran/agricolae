\name{dimnames.order}
\alias{dimnames.order}
%- dimnames.order.
\title{ Order in rownames and colnames }
\description{
It orders the names of the rows and columns of a matrix.
}
\usage{
dimnames.order(x)
}

\arguments{
  \item{x}{ data.frame or Matrix }
}

\value{

  \item{x }{numeric or alphanumeric}
}
\author{ Felipe de Mendiburu }

\examples{
library(agricolae)
data(soil)
rownames(soil)<-soil[,1]
soil<-soil[,-1]
soil<-dimnames.order(soil)
soil
}
\keyword{ manip }% at least one, from doc/KEYWORDS

