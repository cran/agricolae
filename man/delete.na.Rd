\name{delete.na}
\alias{delete.na}
%- delete.na.
\title{ Omits the rows or columns lesser or greater with NA of the matrix }
\description{
  In many situations it is required omits the rows or columns lesser
  or greater with NA of the matrix.
}
\usage{
delete.na(x, alternative=c("less", "greater") )
}

\arguments{
  \item{x}{ matrix with NA }
  \item{alternative}{ "less" or "greater" }
}
\value{
  \item{x }{matrix}
}

\author{ Felipe de Mendiburu }
\examples{
library(agricolae)
x<-c(2,5,3,7,5,NA,8,0,4,3,NA,NA)
dim(x)<-c(4,3)
x
#     [,1] [,2] [,3]
#[1,]    2    5    4
#[2,]    5   NA    3
#[3,]    3    8   NA
#[4,]    7    0   NA

delete.na(x,"less")
#     [,1]
#[1,]    2
#[2,]    5
#[3,]    3
#[4,]    7

delete.na(x,"greater")
#     [,1] [,2] [,3]
#[1,]    2    5    4

}
\keyword{ manip }% at least one, from doc/KEYWORDS

