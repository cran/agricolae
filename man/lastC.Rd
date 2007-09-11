\name{lastC}
\alias{lastC}
%- lastC.
\title{ Setting the last character of a chain  }
\description{
  A special function for the group of treatments in the multiple comparison tests. 
  Use order.group.
}
\usage{
lastC(x)
}

\arguments{
  \item{x}{ letters }
}
\value{
  \item{x }{character}
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{order.group} } }

\examples{
library(agricolae)
x<-c("a","ab","b","c","cd")
lastC(x)
# "a" "b" "b" "c" "d"

}
\keyword{ manip }% at least one, from doc/KEYWORDS

