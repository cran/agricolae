\name{mxyz}
\alias{mxyz}
%- mxyz.
\title{ Mean data frame in matrix}
\description{
  Forms a matrix of a table of data, finds the average. 
  Tool for the stability analysis.
}
\usage{
mxyz(x, y, z)
}

\arguments{
  \item{x}{ Vector }
  \item{y}{ Vector }
  \item{z}{ Vector }
}

\value{
  \item{x}{ Alfanumeric }
  \item{y}{ Alfanumeric }
  \item{z}{ Numeric }
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{stability.par}}, \code{\link{stability.nonpar} } }
\examples{
library(agricolae)
# example 1
z<-1:15
x<-c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)
y<-c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
mxyz(x,y,z)
#
#    1  2  3
# 1  1  2  3
# 2  4  5  6
# 3  7  8  9
# 4 10 11 12
# 5 13 14 15

# Example 2
data(CIC)
attach(CIC)
means<-mxyz(Genotype,Environment,Relative)
CIC.mean<-data.frame(genotype=row.names(means),means)
CIC.mean<-delete.na(CIC.mean,"greater")
stability.nonpar(CIC.mean)

}
\keyword{ manip }% at least one, from doc/KEYWORDS

