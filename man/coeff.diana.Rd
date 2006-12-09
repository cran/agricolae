\name{coeff.diana}
\alias{coeff.diana}
%- coeff.diana.
\title{ Selection of the best method for the calculation of the distance of dissimilarity }
\description{
  Method based on the  divisive coefficient by the method DIANA.
  Uses the methods of distance provided by function DIST of library CLUSTER.
  Also it reports the first selected distance based on highest value of the coefficient.
}
\usage{
coeff.diana(data)
}

\arguments{
  \item{data}{ data.frame }
}
\details{
distance: euclidean, maximum, manhattan, canberra, binary, minkowski.
}
\value{
  \item{data }{each column represents a variable}
}
\references{ Finding Groups in Data. An Introduction to Cluster Analysis. 
Leonard Kaufaman. 1990, Jhon Wiley ans Sons, Inc. }

\author{ Felipe de Mendiburu }

\examples{
library(cluster)
library(agricolae)
# Example 1
data.1 <- rbind(c(1,1,1,1,1),c(1,1,0,1,1),c(0,0,1,0,0),c(1,0,1,0,1),
c(0,0,0,0,0),c(1,1,1,1,1))
dimnames(data.1)<-list(c("A","B","C","D","E","F"),c("m1","m2","m3","m4","m5"))
distance<-coeff.diana(data.1)
h<-hclust(distance,method="mcquitty")
#startgraph
plot(h)
#endgraph

# Example 2
data.2 <- rbind(c(1200,2,6,20),c(1500,1,3,10),c(1400,1,3,20),c(1600,2,5,15),
c(1000,1,2,5))
dimnames(data.2)<-list(c("A","B","C","D","E"),c("v1","v2","v3","v4"))
distance<-coeff.diana(data.2)

# Example 3
data.3 <-rbind(c(4,2,6,15),c(NA,1,3,18),c(2,1,NA,16),c(6,2,5,15),c(NA,1,2,NA),
c(4,2,6,10))
dimnames(data.3)<-list(c("+","+","+",".",".","."),c("x1","x2","x3","x4"))
distance<-coeff.diana(data.3)

}
\keyword{ cluster }% at least one, from doc/KEYWORDS

