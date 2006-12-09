\name{last.c}
\alias{last.c}
%- last.c.
\title{ Last for group }
\description{
  Special function for the group of treatments in the tests of multiple
  comparison, use order.group.
}
\usage{
last.c(x)
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
last.c(x)
# "a" "b" "b" "c" "d"

}
\keyword{ manip }% at least one, from doc/KEYWORDS

