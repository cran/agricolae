\name{similarity}
\alias{similarity}
%- similarity.
\title{ matrix of similarity in binary data }
\description{
 Finds the matrix of similarity of binary tables (1 and 0).
}
\usage{
similarity(A)
}

\arguments{
  \item{A}{ Matrix, data binary }
}
\value{
  \item{A }{Numeric (0,1) }
}
\author{ Felipe de Mendiburu }

\seealso{\code{\link{cv.similarity}}, \code{\link{resampling.cv} } }

\examples{
#example table of molecular markers
library(agricolae)
data(markers)
distance<-similarity(markers)
#startgraph
tree<-hclust(distance,method="mcquitty")
plot(tree,col="blue")
#endgraph
}
\keyword{ models }% at least one, from doc/KEYWORDS

