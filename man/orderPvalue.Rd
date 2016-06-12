\name{orderPvalue}
\alias{orderPvalue}
%- orderPvalue.
\title{ Grouping the treatments averages in a comparison with a minimum value  }
\description{
  When there are treatments and their respective values, these can be 
  compared with a minimal difference of meaning.
}
\usage{
orderPvalue(treatment, means, alpha, pvalue, console)
}

\arguments{
  \item{treatment}{ treatment }
  \item{means}{ means of treatment }
  \item{alpha}{ Alpha value, significante value to comparison }
  \item{pvalue}{ Matrix of probabilities to comparison }
  \item{console}{logical, print output }
}

\value{
  \item{trt}{ Factor }
  \item{means}{ Numeric }
  \item{pvalue}{ matrix }
}

\author{ Felipe de Mendiburu }

\seealso{\code{\link{order.group} } , \code{\link{order.stat} }}

\examples{
library(agricolae)
treatments <- c("A","B","C")
means<-c(2,5,3)
alpha <- 0.05
pvalue<-matrix(1,nrow=3,ncol=3)
pvalue[1,2]<-pvalue[2,1]<-0.03
pvalue[1,3]<-pvalue[3,1]<-0.10
pvalue[2,3]<-pvalue[3,2]<-0.06
groups<-orderPvalue(treatments,means,alpha,pvalue,console=TRUE)

}
\keyword{ manip }% at least one, from doc/KEYWORDS

